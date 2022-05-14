#' Layer operation addition
#'
#' [Operation]s can be added together to form stacks of operations, which
#' when multiplied by (applied to) [Layer]s, those [Layer]s are distributed
#' over the [Operation]s (i.e. copied).
#'
#' @param x,... [Operation]s
#' @param e1 an [Operation] or [numeric()]
#' @param e2 an [Operation] or [numeric()]
#' @param na.rm ignored
#'
#' @details
#' Addition of \pkg{ggblend} [Operation]s depends on the types of
#' objects being summed:
#'
#' - If you add an [Operation] to an [Operation], they are merged into
#'   a single [Operation] that copies input [Layer]s, one for each [Operation].
#' - If you add an [Operation] to a [numeric()] *n*, it is equivalent to
#'   adding `*` [nop()]s to that [Operation].
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # adding operations together creates a sum of operations
#' adjust(color = "red") + adjust(size = 2)
#'
#' # addition and multiplication obey the distributive law
#' op = (adjust(aes(y = 11 -x), color = "skyblue") + 1) * (adjust(color = "white", size = 4) + 1)
#' op
#'
#' # multiplication by a geom returns a modified version of that geom,
#' # distributed over the sum of the operations
#' data.frame(x = 1:10) |>
#'   ggplot(aes(x = x, y = x)) +
#'   geom_line(size = 2) * op
#' @name OperationSum-class
#' @aliases OperationSum
NULL

new_operation_sum = function(list) {
  new("OperationSum", .Data = list)
}

#' @rdname OperationSum-class
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

setMethod("apply_operation", signature(operation = "OperationSum"), function(operation, layers) {
  simplify_layers(
    lapply(operation, apply_operation, layers = layers),
    prototype = layers
  )
})


# operation concatenation -------------------------------------------------

#' @rdname OperationSum-class
#' @export
setMethod("+", signature(e1 = "Operation", e2 = "Operation"), function(e1, e2) {
  e1 = as(e1, "OperationSum")
  e2 = as(e2, "OperationSum")
  new_operation_sum(c(e1, e2))
})

#' @rdname OperationSum-class
#' @export
setMethod("+", signature(e1 = "Operation", e2 = "numeric"), function(e1, e2) {
  e1 + e2 * nop()
})

#' @rdname OperationSum-class
#' @export
setMethod("+", signature(e1 = "numeric", e2 = "Operation"), function(e1, e2) {
  e1 * nop() + e2
})


# printing ----------------------------------------------------------------

#' @rdname OperationSum-class
#' @export
setMethod("format", signature(x = "OperationSum"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    paste(vapply(x, format, character(1)), collapse = " + ")
  }
})
