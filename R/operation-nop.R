#' Identity ("no-op") transformation (Layer Operation)
#'
#' A layer [Operation] which returns the input [Layer] unchanged.
#'
#' @param x One of:
#'  - A [Layer]-like object: applies this operation to the layer.
#'  - A missing argument: creates an [Operation]
#'
#' @details
#'
#' When `numeric()` are used with [Operation]s, they are converted into
#' sums of `nop()`s.
#'
#' @template operation
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # adding a nop to another operation is equivalent to adding a numeric
#' adjust() + nop()
#'
#' # and vice versa
#' adjust() + 2
#'
#' # here we use adjust() with nop() ( + 1) to create a copy of
#' # the stat_smooth layer, putting a white outline around it.
#' set.seed(1234)
#' k = 1000
#' data.frame(
#'   x = seq(1, 10, length.out = k),
#'   y = rnorm(k, seq(1, 2, length.out = k) + c(0, 0.5)),
#'   g = c("a", "b")
#' ) |>
#'   ggplot(aes(x, y, color = g)) +
#'   geom_point() +
#'   stat_smooth(method = lm, formula = y ~ x, size = 1.5, se = FALSE) *
#'     (adjust(aes(group = g), color = "white", size = 4) + 1) +
#'   scale_color_brewer(palette = "Dark2")
#'
#' # (note this could also be done with copy_under())
#'
#' @name nop
NULL

new_nop = function() {
  new("Nop")
}

#' @rdname nop
#' @export
nop = make_operation("nop", new_nop)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "Nop"), function(operation, layers) {
  simplify_layers(layers, prototype = layers)
})


# operation multiplication ------------------------------------------------

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "Operation", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "Operation"), function(e1, e2) e2)

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "OperationSum"), function(e1, e2) e2)


# printing ----------------------------------------------------------------

#' @rdname Operation-class
#' @export
setMethod("format", signature(x = "Nop"), function(x, ...) {
  "1"
})
