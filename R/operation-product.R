new_operation_product = function(list) {
  new("operation_product", .Data = list)
}

#' @rdname operation_product
#' @export
setMethod("prod", signature(x = "operation"), function(x, ..., na.rm = FALSE) {
  new_operation_product(list(x, ...))
})


# type conversion ---------------------------------------------------------

setAs("list", "operation_product", function(from) {
  new_operation_product(lapply(from, as, "operation"))
})

setAs("operation", "operation_product", function(from) new_operation_product(list(from)))


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "operation_product"), function(operation, layers) {
  Reduce(`*`, operation@.Data, layers)
})


# operation multiplication -------------------------------------------------

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation", e2 = "operation"), function(e1, e2) {
  e1 = as(e1, "operation_product")
  e2 = as(e2, "operation_product")
  new_operation_product(c(e1, e2))
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "numeric", e2 = "operation"), function(e1, e2) {
  e2 = as(e2, "operation_product")
  operations = rep(e2@.Data, times = e1)
  new_operation_sum(operations)
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation", e2 = "numeric"), function(e1, e2) {
  e2 * e1
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation", e2 = "operation_sum"), function(e1, e2) {
  new_operation_sum(lapply(e2, `*`, e1 = e1))
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation_sum", e2 = "operation"), function(e1, e2) {
  new_operation_sum(lapply(e1, `*`, e2 = e2))
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation_sum", e2 = "operation_sum"), function(e1, e2) {
  new_operation_sum(unlist(lapply(e2, `*`, e1 = e1)))
})


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "operation_product"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    paste(vapply(x, format, character(1)), collapse = " * ")
  }
})
