new_affine_transform = function(x = 0, y = 0, width = 1, height = 1, angle = 0) {
  x = check_unit(x)
  y = check_unit(y)
  width = check_unit(width)
  height = check_unit(height)

  new("affine_transform", x = x, y = y, width = width, height = height, angle = angle)
}

#' @rdname affine_transform
#' @export
affine_transform = make_operation("affine_transform", new_affine_transform, x)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "affine_transform"), function(operation, layers) {
  grob_transform = function(grob) affine_transform_grob(
    grob,
    x = operation@x, y = operation@y,
    width = operation@width, height = operation@height,
    angle = operation@angle
  )
  layer_apply(layers, transform_layer, grob_transform = grob_transform, check = check_affine_transform)
})

setMethod("apply_composed_operation", signature(operation = "affine_transform", layers = "list"), function(operation, layers) {
  grob_transform = function(grob) affine_transform_grob(
    grob,
    x = operation@x, y = operation@y,
    width = operation@width, height = operation@height,
    angle = operation@angle
  )
  transform_layers(layers, grob_transform, check = check_affine_transform)
})


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "affine_transform"), function(x, ...) {
  defaults = list(x = unit(0, "npc"), y = unit(0, "npc"), width = unit(1, "npc"), height = unit(1, "npc"), angle = 0)
  args_string = format_name_value_pairs(attributes(x)[names(defaults)], defaults)
  paste0(tolower(class(x)), "(", args_string, ")")
})


# affine transform for grobs -----------------------------------------------------

#' Apply affine transform to a grob
#' @param x x translation (a unit)
#' @param y y translation (a unit)
#' @param width width (a unit)
#' @param height height (a unit)
#' @param angle angle of rotation (a numeric)
#' @return a grob
#' @noRd
affine_transform_grob = function(grob, x, y, width, height, angle) {
  dg = defineGrob(grob)

  grobTree(dg,
    grobTree(
      useGrob(dg$name),
      vp = viewport(
        x = unit(0.5, "npc") + x, y = unit(0.5, "npc") + y,
        width = width, height = height, angle = angle
      )
    )
  )
}

#' check that an argument is a `unit()`
#' @param arg argument that should be a `unit()`
#' @noRd
check_unit = function(arg) {
  if (is.unit(arg)) {
    arg
  } else if (is.numeric(arg)) {
    unit(arg, "npc")
  } else {
    warning0(
      deparse1(substitute(arg)), " argument to affine_transform() has class ",
      deparse1(class(arg)), ". It must be a numeric or a grid::unit()."
    )
  }
}

#' Check to see if transformations are supported by the current
#' graphics device, issuing a warning if not
#' @noRd
check_affine_transform = function() {
  if (
    getOption("ggblend.check_affine_transform", TRUE) &&
    grDevices::dev.cur() != 1 &&
    !isTRUE(grDevices::dev.capabilities()$transformations)
  ) {
    warning0(
      'Your graphics device, ', deparse1(names(grDevices::dev.cur())),
      ', reports that affine transformations are not supported.\n',
      bullet('If the transformed output IS NOT as expected (e.g. geoms are not being
        drawn), then you must switch to a graphics device that supports transformations,
        like png(type = "cairo"), svg(), or cairo_pdf().
      '), "\n",
      bullet('If the transformed output IS as expected despite this warning, this is likely a
        bug *in the graphics device*. Unfortunately, several graphics do not correctly
        report their capabilities. You may wish to a report a bug to the authors of the
        graphics device. In the mean time, you can disable this
        warning via options(ggblend.check_affine_transform = FALSE).
      '), "\n",
      bullet("For more information, see the Supported Devices section of help('affine_transform').")
    )
  }
  invisible(NULL)
}
