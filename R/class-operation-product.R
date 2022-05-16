#' Layer operation products
#'
#' [operation]s can be multiplied together to form chains of operations, which
#' when multiplied by (applied to) [layer-like] objects, return modified [layer-like] objects.
#'
#' @param x,... [operation]s
#' @param e1 an [operation], [layer-like], or [numeric()]
#' @param e2 an [operation], [layer-like], or [numeric()]
#' @param na.rm ignored
#'
#' @details
#' Multiplication of \pkg{ggblend} [operation]s depends on the types of
#' objects being multiplied:
#'
#' - If you multiply an [operation] with an [operation], they are merged into
#'   a single [operation] that applies each [operation] in sequence.
#' - If you multiply an [operation] with a [layer-like] object, that operation is applied
#'   to the layer, returning a new [layer-like] object.
#' - If you multiply an [operation] by a [numeric()] *n*, a new [operation] that
#'   repeats the input [operation] is *n* times is returned.
#'
#' @examples
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
#'
#' @name operation_product
#' @aliases operation_product-class
#' @export
setClass("operation_product", contains = c("list", "operation"))
