#' helper for running tests without warnings about graphics device support
without_warnings = function(expr) {
  old_options = options(
    "ggblend.check_blend" = FALSE,
    "ggblend.check_affine_transform" = FALSE
  )

  expr

  options(old_options)
}

#' helper for running tests in a graphics device that should throw warnings
with_old_graphics_device = function(expr) {
  path = tempfile(fileext = ".tex")
  pictex(path)
  on.exit(dev.off())

  expr
}
