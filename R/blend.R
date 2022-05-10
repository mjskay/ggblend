#' Blend ggplot2 layers
#'
#' Blend objects within a single layer (geom) or across multiple layers (geoms)
#' using graphical blending modes, such as `"multiply"`, `"overlay"`, etc. Uses
#' the built-in compositing support in graphical devices added in R 4.2.
#'
#' @param x One of:
#'   - A `ggplot2::Layer`, such as a `geom` or `stat`, or a list of layers.
#'   - A string (character vector of length 1) giving the name of a blend,
#'     which takes the place of the `blend` argument.
#'
#'  If a layer is provided, this stack of blends is applied to it.
#'  If a string is provided, this takes the place of the `blend` argument, and
#'  a `"ggblend"` object is returned which can be applied using the
#'  [`*.ggblend`] and [`^.ggblend`] operators.
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
#'  Blend modes like `"multiply"`, `"darken"`, and `"lighten"` are particularly useful as they
#'  are *commutative*: the result is the same whichever order they are applied in.
#'
#'  A warning is issued if the current graphics device does not appear to support
#'  the requested blend mode. In some cases this warning may be spurious, so
#'  it can be disabled by setting `options(ggblend.check_blend = FALSE)`.
#' @param alpha A numeric between `0` and `1` (inclusive). The opacity of a
#'  transparency mask applied to objects prior to blending.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' If `x` is a single layer / geometry and the `blend_group` aesthetic *is not* set, every
#' graphical object ([grob()]) output by the geometry will be blended together
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
#' One of:
#'
#'  - An object that can be added to a [ggplot()] object: if the input is a `ggplot2::Layer`,
#'    the result is a `ggplot2::Layer`; if the input is a list of `ggplot2::Layer`s,
#'    the output is a list of `ggplot2::Layer`s.
#'  - If the input is a string, a `"ggblend"` object, which can be combined with other `"ggblend"`
#'    objects using [`^.ggblend`] or applied to a ggplot layer using [`*.ggblend`].
#'
#' @references
#' Murrell, Paul (2021):
#' [Groups, Compositing Operators, and Affine Transformations in R Graphics](https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html).
#' The University of Auckland. Report.
#' \doi{10.17608/k6.auckland.17009120.v1}.
#'
#' @family blending functions and operators
#'
#' @examples
#' library(ggplot2)
#'
#' # create two versions of a dataset, where draw order can affect output
#' set.seed(1234)
#' df_a = data.frame(x = rnorm(500, 0), y = rnorm(500, 1), set = "a")
#' df_b = data.frame(x = rnorm(500, 1), y = rnorm(500, 2), set = "b")
#' df_ab = rbind(df_a, df_b) |>
#'   transform(order = "draw a then b")
#' df_ba = rbind(df_b, df_a) |>
#'   transform(order = "draw b then a")
#' df = rbind(df_ab, df_ba)
#'
#' # Using the "darken" blend mode, draw order does not matter:
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) |> blend("darken") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # Using the "multiply" blend mode, we can see density within groups:
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) |> blend("multiply") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # blend() on a single geom by default blends all grobs in that geom together
#' # using the requested blend mode. If we wish to blend within specific data
#' # subsets using normal blending ("over") but between subsets using the
#' # requested blend mode, we can set the blend_group aesthetic. This will
#' # make "multiply" behave more like "darken":
#' df |>
#'   ggplot(aes(x, y, color = set, blend_group = set)) +
#'   geom_point(size = 3) |> blend("multiply") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # We can also blend lists of geoms together; these geoms are rendered using
#' # normal ("over") blending (unless a blend() call is applied to a specific
#' # sub-layer, as in the first layer below) and then blended together using
#' # the requested blend mode.
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   list(
#'     geom_point(size = 3) |> blend("darken"),
#'     geom_vline(xintercept = 0, color = "gray75", size = 1.5),
#'     geom_hline(yintercept = 0, color = "gray75", size = 1.5)
#'   ) |> blend("hard.light") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' @export
blend = function(x, blend = "over", alpha = 1, ...) {
  UseMethod("blend")
}

#' @rdname blend
#' @export
blend.Layer = function(x, blend = "over", alpha = 1, ...) {
  check_blend(blend)
  blend_layer(x, blend, alpha)
}

#' @rdname blend
#' @export
blend.list = function(x, blend = "over", alpha = 1, ...) {
  # this is kind of hacky but seems to work --- basically, make a list of
  # layers where all the input layers are hidden layers (when $draw_geom() is
  # called on them it saves the data needed for drawing but otherwise does
  # nothing), and the final layer actually draws each layer and then blends
  # them together
  check_blend(blend)
  blend_layers(x, blend, alpha)
}

#' @rdname blend
#' @export
blend.character = function(x, blend, alpha = 1, ...) {
  if (!missing(blend)) {
    stop0("Cannot provide both `x` and `blend` when `x` is a string in blend().")
  }
  new_ggblend(blend = x, alpha = alpha)
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
  ggproto("HiddenLayer", layer,
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
  if (getOption("ggblend.check_blend", TRUE) && !isTRUE(blend %in% grDevices::dev.capabilities()$compositing)) {
    warning0(
      'blend = ', deparse1(blend), ' does not appear to be supported by your graphics device.\n',
      ' - Blending output may not be as expected.\n',
      ' - If your current graphics device *does* support blend = ', deparse1(blend), '\n',
      '   but auto-detection failed, consider reporting a bug.'
    )
  }
  invisible(blend)
}
