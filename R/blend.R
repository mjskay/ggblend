#' Blend ggplot2 layers
#'
#' Blend objects within a single layer (geom) or across multiple layers (geoms)
#' using graphical blending modes, such as `"multiply"`, `"overlay"`, etc. Uses
#' the built-in compositing support in graphical devices added in R 4.2.
#'
#' @param x A `ggplot2::Layer`, such as a `geom` or `stat`, or a list
#'  of layers.
#' @param blend The blend mode to use. The default mode, `"over"`, corresponds to
#'  the "usual" blend mode of drawing objects on top of each other.
#'  The list of supported blend modes depends on your graphical device
#'  (see Murrell 2021), and are listed in `dev.capabilities()$compositing`.
#'  Blend modes can include: `"clear"`,
#'  `"source"`, `"over"`, `"in"`, `"out"`, `"atop"`, `"dest"`, `"dest.over"`,
#'  `"dest.in"`, `"dest.out"`, `"dest.atop"`, `"xor"`, `"add"`, `"saturate"`,
#'  `"multiply"`, `"screen"`, `"overlay"`, `"darken"`, `"lighten"`,
#'  `"color.dodge"`, `"color.burn"`, `"hard.light"`, `"soft.light"`,
#'  `"difference"`, and `"exclusion"`
#'
#'  Blend modes like `"multiply"` and `"screen"` are particularly useful as they
#'  are *commutative*: the result is the same whichever order they are applied in.
#' @param alpha A numeric between `0` and `1` (inclusive). The opacity of a
#'  transparency mask applied to objects prior to blending.
#'
#' @details
#'
#' If `x` is a single layer / geometry and the `blend_group` aesthetic *is not* set, every
#' graphical object (`grob()`) output by the geometry will be blended together
#' using the `blend` blend mode. If `alpha != 1`, a transparency mask with the
#' provided alpha level will be applied to each grob before blending.
#'
#' If `x` is a single layer / geometry and the `blend_group` aesthetic *is* set,
#' the geometry will be rendered for each subset of the data defined by the
#' `blend_group` aesthetic, a transparency mask with the provided `alpha` level
#' will be applied to each resulting group as a whole (if `alpha != 1`), then these groups
#' will be blended together using the `blend` blend mode.
#'
#' If `x` is a list of layers / geometries, those layers will be rendered
#' separately, a transparency mask with the provided `alpha` level
#' will be applied to each layer as a whole (if `alpha != 1`), then these layers
#' will be blended together using the `blend` blend mode.
#'
#' @return
#' An object that can be added to a `ggplot()` object: if the input is a `ggplot2::Layer`,
#' the result is a `ggplot2::Layer`; if the input is a `list` of `ggplot2::Layer`s,
#' the output is a `list` of `ggplot2::Layer`s.
#'
#' @references
#'
#' Murrell, Paul (2021):
#' [Groups, Compositing Operators, and Affine Transformations in R Graphics](https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html).
#' The University of Auckland. Report.
#' \doi{10.17608/k6.auckland.17009120.v1}.
#'
#' @export
blend = function(x, blend = "over", alpha = 1, ...) {
  check_blend(blend)
  UseMethod("blend")
}

#' @export
blend.Layer = function(x, blend = "over", alpha = 1, ...) {
  blend_layer(x, blend, alpha)
}

#' @export
blend.list = function(x, blend = "over", alpha = 1, ...) {
  # this is kind of hacky but seems to work --- basically, make a list of
  # layers where all the input layers are hidden layers (when $draw_geom() is
  # called on them it saves the data needed for drawing but otherwise does
  # nothing), and the final layer actually draws each layer and then blends
  # them together
  layers = lapply(x, hide_layer)
  c(layers, list(blend_layers(layers, blend, alpha)))
}


# helpers -----------------------------------------------------------------

#' Blend a grob
#' @param grob grob to blend
#' @param blend a blend mode
#' @param alpha alpha of a transparency mask to be applied to each grob before the blend
#' @return a grob
#' @noRd
blend_grob = function(grob, blend = "over", alpha = 1) {
  viewport = if (!isTRUE(alpha == 1)) {
    mask = rectGrob(gp = gpar(col = NA, fill = rgb(0, 0, 0, alpha)))
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
  layer = geom_blank()
  ggproto(NULL, layer,
    draw_geom = function(self, data, layout) {
      groblists = lapply(layers, function(l) {
        groblist = l$ggblend__draw_geom_(layout)
        # do not blend within layers
        lapply(groblist, groupGrob)
      })
      blend_groblists(groblists, blend, alpha)
    }
  )
}

#' Make a layer that will blend its contents when created. If the layer does not
#' have a blend_group aesthetic, blend all its grobs together. If it does,
#' first generate grobs for each blend group, then blend the groups together.
#' @param layer a ggplot2::Layer
#' @param blend blend mode
#' @noRd
blend_layer = function(layer, blend = "over", alpha = 1) {
  ggproto(NULL, layer,
    draw_geom = function(self, data, layout) {
      if (is.null(data$blend_group)) {
        # absent a blend_group aes, we apply the blend to all grobs in the layer
        groblist = ggproto_parent(layer, self)$draw_geom(data, layout)
        lapply(groblist, blend_grobs, blend = blend, alpha = alpha)
      } else {
        # with a blend_group aes, we apply the blend between blend groups

        # draw the geom for each blend_group separately
        # groblists will be a list of groblists, where each groblist is a list
        # of grobs for a single layer
        groblists = lapply(split(data, data$blend_group), function(d) {
          groblist = ggproto_parent(layer, self)$draw_geom(d, layout)
          # make layers their own blend group so that the blend is only
          # applied between layers, not within layers
          lapply(groblist, groupGrob)
        })

        blend_groblists(groblists, blend, alpha)
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
  ggproto(NULL, layer,
    # keep ggplot from drawing this layer normally, we will draw it later
    draw_geom = function(self, data, layout) {
      self$ggblend__last_data_ = data
      ggproto_parent(layer, self)$draw_geom(data[NULL,], layout)
    },
    # function to actually draw the geom, using the data saved from the last
    # call to $draw_geom()
    ggblend__draw_geom_ = function(self, layout) {
      ggproto_parent(layer, self)$draw_geom(self$ggblend__last_data_, layout)
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
  if (options("ggblend.check_blend", TRUE) && !isTRUE(blend %in% grDevices::dev.capabilities()$compositing)) {
    warning0(
      'blend = ', deparse1(blend), ' does not appear to be supported by your graphics device.\n',
      ' - Blending output may not be as expected.\n',
      ' - If your current graphics device *does* support blend = ', deparse1(blend), '\n',
      '   but auto-detection failed, consider reporting a bug.'
    )
  }
  invisible(blend)
}
