#' Layer operation multiplication and application
#'
#' [Operation]s can be multiplied together to form chains of operations, which
#' when multiplied by (applied to) [Layer]s, return modified [Layer]s.
#'
#' @param x,... [Operation]s
#' @param e1 an [Operation], [Layer], or [numeric()]
#' @param e2 an [Operation], [Layer], or [numeric()]
#' @param na.rm ignored
#'
#' @details
#' Multiplication of \pkg{ggblend} [Operation]s depends on the types of
#' objects being multiplied:
#'
#' - If you multiply an [Operation] with an [Operation], they are merged into
#'   a single [Operation] that applies each [Operation] in sequence.
#' - If you multiply an [Operation] with a [Layer], that operation is applied
#'   to the layer, returning a new [Layer].
#' - If you multiply an [Operation] by a [numeric()] *n*, a new [Operation] that
#'   repeats the input [Operation] is *n* times is returned.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # multiplying operations by numerics repeats them...
#' adjust(color = "red") * 2
#'
#' # multiplying operations together chains (or merges) them
#' adjust(color = "red") * adjust(size = 2)
#'
#' # multiplication obeys the distributive law
#' op = (adjust(aes(y = 11 -x), color = "skyblue") + 1) * (adjust(color = "white", size = 4) + 1)
#' op
#'
#' # multiplication by a geom returns a modified version of that geom
#' data.frame(x = 1:10) |>
#'   ggplot(aes(x = x, y = x)) +
#'   geom_line(size = 2) * op
#' @name OperationProduct-class
#' @aliases OperationProduct
NULL

new_operation_product = function(list) {
  new("OperationProduct", .Data = list)
}

#' @rdname OperationProduct-class
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

setMethod("apply_operation", signature(operation = "OperationProduct"), function(operation, layers) {
  Reduce(`*`, operation@.Data, layers)
})


# operation multiplication -------------------------------------------------

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "Operation", e2 = "Operation"), function(e1, e2) {
  e1 = as(e1, "OperationProduct")
  e2 = as(e2, "OperationProduct")
  new_operation_product(c(e1, e2))
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "numeric", e2 = "Operation"), function(e1, e2) {
  e2 = as(e2, "OperationProduct")
  operations = rep(e2@.Data, times = e1)
  new_operation_sum(operations)
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "Operation", e2 = "numeric"), function(e1, e2) {
  e2 * e1
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "Operation", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(lapply(e2, `*`, e1 = e1))
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "Operation"), function(e1, e2) {
  new_operation_sum(lapply(e1, `*`, e2 = e2))
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(unlist(lapply(e2, `*`, e1 = e1)))
})


# printing ----------------------------------------------------------------

#' @rdname Operation-class
#' @export
setMethod("format", signature(x = "OperationProduct"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    paste(vapply(x, format, character(1)), collapse = " * ")
  }
})
