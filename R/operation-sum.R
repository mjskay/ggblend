new_operation_sum = function(list) {
  new("OperationSum", .Data = list)
}

#' @export
setMethod("sum", signature(x = "Operation"), function(x, ..., na.rm = FALSE) {
  as(list(x, ...), "OperationSum")
})


# type conversion ---------------------------------------------------------

setAs("list", "OperationSum", function(from) {
  new_operation_sum(lapply(from, as, "Operation"))
})

setAs("Operation", "OperationSum", function(from) new_operation_sum(list(from)))


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "OperationSum"), function(operation, layers) {
  simplify_layers(
    lapply(operation, apply_operation, layers = layers),
    prototype = layers
  )
})


# operation concatenation -------------------------------------------------

#' @export
setMethod("+", signature(e1 = "Operation", e2 = "Operation"), function(e1, e2) {
  e1 = as(e1, "OperationSum")
  e2 = as(e2, "OperationSum")
  new_operation_sum(c(e1, e2))
})

#' @export
setMethod("+", signature(e1 = "Operation", e2 = "numeric"), function(e1, e2) {
  e1 + e2 * nop()
})

#' @export
setMethod("+", signature(e1 = "numeric", e2 = "Operation"), function(e1, e2) {
  e1 * nop() + e2
})


# printing ----------------------------------------------------------------

#' @export
setMethod("format", signature(x = "OperationSum"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    paste(vapply(x, format, character(1)), collapse = " + ")
  }
})
