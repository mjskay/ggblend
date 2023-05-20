unname_grob = function(x) {
  if (is.list(x)) {
    out = sapply(x, unname_grob, simplify = FALSE)
    class(out) = class(x)
  } else {
    out = x
  }
  if (inherits(out, "grob")) {
    out$name = NULL
    out$childrenOrder = NULL
    names(out$children) = NULL
  }
  out
}

#' used for comparing grobs. Grobs may not appear to be exactly equal because
#' of names; this expectation fixes that
expect_equal_grob = function(object, expected) {
  label = as_label(enquo(object))
  expected.label = as_label(enquo(expected))

  expect_equal(
    unname_grob(object), unname_grob(expected),
    label = label,
    expected.label = expected.label
  )
}
