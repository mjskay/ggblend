new_blend = function(blend = "over", alpha = 1) {
  check_blend(blend)

  new("blend", blend = blend, alpha = alpha)
}

#' @rdname blend
#' @export
blend = make_operation("blend", new_blend, blend)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "blend"), function(operation, layers) {
  layer_apply(layers, blend_layer, blend = operation@blend, alpha = operation@alpha)
})

setMethod("apply_composed_operation", signature(operation = "blend", layers = "list"), function(operation, layers) {
  # this is kind of hacky but seems to work --- basically, make a list of
  # layers where all the input layers are hidden layers (when $draw_geom() is
  # called on them it saves the data needed for drawing but otherwise does
  # nothing), and the final layer actually draws each layer and then blends
  # them together
  blend_layers(layers, operation@blend, operation@alpha)
})


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "blend"), function(x, ...) {
  blend_string = if (x@blend != "over") deparse1(x@blend)
  alpha_string = if (x@alpha != 1) paste0("alpha = ", deparse1(x@alpha))
  args_string = paste0(c(blend_string, alpha_string), collapse = ", ", recycle0 = TRUE)
  paste0(tolower(class(x)), "(", args_string, ")")
})


# helpers -----------------------------------------------------------------

#' Blend a grob
#' @param grob grob to blend
#' @param blend a blend mode
#' @param alpha alpha of a transparency mask to be applied to each grob before the blend
#' @return a grob
#' @noRd
blend_grob = function(grob, blend = "over", alpha = 1) {
  viewport = if (!isTRUE(alpha == 1)) {
    mask = rectGrob(gp = gpar(col = NA, fill = grDevices::rgb(0, 0, 0, alpha)))
    viewport(mask = mask)
  }
  groupGrob(grob, blend, vp = viewport)
}

#' Blend grobs together
#' @param ... grobs to blend
#' @param blend a blend mode
#' @return a grob
#' @noRd
blend_grobs = function(..., blend = "over", alpha = 1) {
  blend_grob(grobTree(...), blend, alpha)
}

#' Blend groblists together. Each groblist represents the grobs for a layer,
#' and is lists of grobs, one for each panel.
#' @param groblists a list of groblists. Each groblist is a list of grobs.
#' @param blend a blend mode
#' @return a single groblist
#' @noRd
blend_groblists = function(groblists, blend = "over", alpha = 1) {
  if (length(groblists) == 0) return(list(zeroGrob()))

  groblist = .mapply(blend_grobs, groblists, list(blend = blend, alpha = alpha))
  names(groblist) = names(groblists[[1]])
  groblist
}

#' Blend layers together
#' @param groblists a list of ggplot2::Layers. Should be hidden layers (as
#' returned by hidden_layer()).
#' @param blend a blend mode
#' @return a ggplot2::Layer
#' @noRd
blend_layers = function(layers, blend = "over", alpha = alpha) {
  # skip over hidden layers when blending (they should already be incorporated
  # into a BlendedLayer)
  layers = unlist(layers)
  already_hidden = vapply(layers, inherits, what = "HiddenLayer", logical(1))
  layers[!already_hidden] = lapply(layers[!already_hidden], hide_layer)
  layers_to_blend = layers[!already_hidden]

  blended_layer = ggproto("BlendedLayer", geom_blank(),
    draw_geom = function(self, data, layout) {
      groblists = lapply(layers_to_blend, function(l) {
        groblist = l$ggblend__draw_geom_(layout)
        # do not blend within layers
        lapply(groblist, groupGrob)
      })
      blend_groblists(groblists, blend, alpha)
    }
  )

  c(layers, list(blended_layer))
}

#' Make a layer that will blend its contents when created. If the layer does not
#' have a partition aesthetic, blend all its grobs together. If it does,
#' first generate grobs for each blend group, then blend the groups together.
#' @param layer a ggplot2::Layer
#' @param blend blend mode
#' @noRd
blend_layer = function(layer, blend = "over", alpha = 1) {
  force(layer)

  ggproto(NULL, layer,
    draw_geom = function(self, data, layout) {
      if (is.null(data$partition)) {
        # absent a partition aes, we apply the blend to all grobs in the layer
        groblist = ggproto_parent(layer, self)$draw_geom(data, layout)
        lapply(groblist, blend_grobs, blend = blend, alpha = alpha)
      } else {
        # with a partition aes, we apply the blend between blend groups

        # draw the geom for each partition separately
        # groblists will be a list of groblists, where each groblist is a list
        # of grobs for a single layer
        groblists = lapply(split(data, data$partition), function(d) {
          groblist = ggproto_parent(layer, self)$draw_geom(d, layout)
          # make layers their own blend group so that the blend is only
          # applied between layers, not within layers
          lapply(groblist, blend_grob)
        })

        blend_groblists(groblists, blend, alpha = alpha)
      }
    }
  )
}

#' Make a hidden layer that does not draw itself by default. We will draw the
#' layer ourselves during blending.
#' @param a ggplot2::Layer to hide
#' @return a ggplot2::Layer whose `$draw_geom()` method does not draw, but
#' saves data for later drawing. Use `$ggblend__draw_geom_()` later to draw.
#' @noRd
hide_layer = function(layer) {
  force(layer)
  store = environment()

  ggproto("HiddenLayer", layer,
    # keep ggplot from drawing this layer normally, we will draw it later
    draw_geom = function(self, data, layout) {
      store$last_grobs = ggproto_parent(layer, self)$draw_geom(data, layout)

      # draw nothing here
      rep(list(zeroGrob()), nrow(layout$layout))
    },
    # function to actually draw the geom, using the grobs saved from the last
    # call to $draw_geom()
    ggblend__draw_geom_ = function(self, layout) {
      store$last_grobs
    }
  )
}

#' Check to see if blending (compositing) is supported by the current
#' graphics device, issuing a warning if it is not
#' @param blend Blend mode to check for. One of:
#'  - `NULL`: disable blending
#'  - A string representing a compositing operator that can be passed to the
#'    `op` argument of `grid::groupGrob()`
#' @return `blend` (invisibly)
#' @noRd
check_blend = function(blend) {
  if (
    getOption("ggblend.check_blend", TRUE) &&
    dev.cur() != 1 &&
    !isTRUE(blend %in% grDevices::dev.capabilities()$compositing)
  ) {
    warning0(
      'blend = ', deparse1(blend), ' does not appear to be supported by your graphics device.\n',
      ' - Blending output may not be as expected.\n',
      ' - If your current graphics device *does* support blend = ', deparse1(blend), '\n',
      '   but auto-detection failed, consider reporting a bug.'
    )
  }
  invisible(blend)
}
