new_operation_product = function(list) {
  new("OperationProduct", .Data = list)
}

#' @export
setMethod("prod", signature(x = "Operation"), function(x, ..., na.rm = FALSE) {
  new_operation_product(list(x, ...))
})


# type conversion ---------------------------------------------------------

setAs("list", "OperationProduct", function(from) {
  new_operation_product(lapply(from, as, "Operation"))
})

setAs("Operation", "OperationProduct", function(from) new_operation_product(list(from)))


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "OperationProduct"), function(operation, layers) {
  Reduce(`*`, operation@.Data, layers)
})


# operation multiplication -------------------------------------------------

#' @export
setMethod("*", signature(e1 = "Operation", e2 = "Operation"), function(e1, e2) {
  e1 = as(e1, "OperationProduct")
  e2 = as(e2, "OperationProduct")
  new_operation_product(c(e1, e2))
})

#' @export
setMethod("*", signature(e1 = "numeric", e2 = "Operation"), function(e1, e2) {
  e2 = as(e2, "OperationProduct")
  operations = rep(e2@.Data, times = e1)
  new_operation_sum(operations)
})

#' @export
setMethod("*", signature(e1 = "Operation", e2 = "numeric"), function(e1, e2) {
  e2 * e1
})

#' @export
setMethod("*", signature(e1 = "Operation", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(lapply(e2, `*`, e1 = e1))
})

#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "Operation"), function(e1, e2) {
  new_operation_sum(lapply(e1, `*`, e2 = e2))
})

#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(lapply(e2, `*`, e1 = e1))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("format", signature(x = "OperationProduct"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    paste(vapply(x, format, character(1)), collapse = " * ")
  }
})
