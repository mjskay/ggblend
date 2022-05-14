#' used for comparing layers. Layers (as ggproto objects) may not appear to be
#' exactly equal because the definitions of variables may be in superclasses;
#' this expectation fixes that
expect_equal_layer = function(object, expected, ...) {
  expect_equal(as.list(object), as.list(expected), ...)
}
