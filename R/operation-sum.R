new_operation_sum = function(list) {
  new("operation_sum", .Data = list)
}

#' @rdname operation_sum
#' @export
setMethod("sum", signature(x = "operation"), function(x, ..., na.rm = FALSE) {
  as(list(x, ...), "operation_sum")
})


# type conversion ---------------------------------------------------------

setAs("list", "operation_sum", function(from) {
  new_operation_sum(do.call(c, lapply(from, as, "operation")))
})

setAs("operation", "operation_sum", function(from) new_operation_sum(list(from)))


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "operation_sum"), function(operation, layers) {
  layer_apply(layers, function(layer) {
    lapply(operation, apply_operation, layers = layer)
  })
})


# operation concatenation -------------------------------------------------

#' @rdname operation_sum
#' @export
setMethod("+", signature(e1 = "operation", e2 = "operation"), function(e1, e2) {
  e1 = as(e1, "operation_sum")
  e2 = as(e2, "operation_sum")
  new_operation_sum(c(e1, e2))
})

#' @rdname operation_sum
#' @export
setMethod("+", signature(e1 = "operation", e2 = "numeric"), function(e1, e2) {
  e1 + e2 * nop()
})

#' @rdname operation_sum
#' @export
setMethod("+", signature(e1 = "numeric", e2 = "operation"), function(e1, e2) {
  e1 * nop() + e2
})


# printing ----------------------------------------------------------------

#' @rdname operation_sum
#' @export
setMethod("format", signature(x = "operation_sum"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else if (length(x) == 1) {
    format(x[[1]])
  } else {
    paste0("(", paste(vapply(x, format, character(1)), collapse = " + "), ")")
  }
})
