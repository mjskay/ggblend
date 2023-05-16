layer_to_list = function(x) {
  x = if (is.list(x)) {
    lapply(x, layer_to_list)
  } else {
    as.list(x)
  }
  # the `constructor` element of a layer may be different depending on the
  # operations that produced it, and we don't care about this when checking
  # for equality
  x$constructor = NULL
  x
}

#' used for comparing layers. Layers (as ggproto objects) may not appear to be
#' exactly equal because the definitions of variables may be in superclasses;
#' this expectation fixes that
expect_equal_layer = function(object, expected) {
  label = as_label(enquo(object))
  expected.label = as_label(enquo(expected))

  expect_equal(
    class(object), class(expected),
    label = paste0("class(", label, ")"),
    expected.label = paste0("class(", expected.label, ")")
  )
  expect_equal(
    layer_to_list(object), layer_to_list(expected),
    label = label,
    expected.label = expected.label
  )
}
