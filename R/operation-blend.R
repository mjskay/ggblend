new_blend = function(blend = "over", alpha = 1) {
  check_blend(blend)

  new("blend", blend = blend, alpha = alpha)
}

#' @rdname blend
#' @export
blend = make_operation("blend", new_blend, blend)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "blend"), function(operation, layers) {
  grob_transform = function(grob) blend_grob(grob, blend = operation@blend, alpha = operation@alpha)
  layer_apply(layers, transform_layer, grob_transform = grob_transform)
})

setMethod("apply_composed_operation", signature(operation = "blend", layers = "list"), function(operation, layers) {
  # this is kind of hacky but seems to work --- basically, make a list of
  # layers where all the input layers are hidden layers (when $draw_geom() is
  # called on them it saves the data needed for drawing but otherwise does
  # nothing), and the final layer actually draws each layer and then blends
  # them together
  grob_transform = function(grob) blend_grob(grob, blend = operation@blend, alpha = operation@alpha)
  transform_layers(layers, grob_transform)
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


# blending grobs ----------------------------------------------------------

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
    grDevices::dev.cur() != 1 &&
    !isTRUE(blend %in% grDevices::dev.capabilities()$compositing)
  ) {
    warning0(
      'Your graphics device, ', deparse1(names(grDevices::dev.cur())),
      ', reports that blend = ', deparse1(blend), ' is not supported.\n',
      bullet('If the blending output IS NOT as expected (e.g. geoms are not being
        drawn), then you must switch to a graphics device that supports blending,
        like png(type = "cairo"), svg(), or cairo_pdf().
      '), "\n",
      bullet('If the blending output IS as expected despite this warning, this is likely a
        bug *in the graphics device*. Unfortunately, several graphics do not correctly
        report their capabilities. You may wish to a report a bug to the authors of the
        graphics device. In the mean time, you can disable this
        warning via options(ggblend.check_blend = FALSE).
      '), "\n",
      bullet("For more information, see the Supported Devices section of help('blend').")
    )
  }
  invisible(blend)
}
