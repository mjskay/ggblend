#' helper for running tests without warnings about graphics device support
without_warnings = function(expr) {
  old_options = options(
    "ggblend.check_blend" = FALSE,
    "ggblend.check_affine_transform" = FALSE
  )

  expr

  options(old_options)
}
