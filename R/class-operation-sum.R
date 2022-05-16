#' Layer operation sums
#'
#' [operation]s can be added together to form stacks of operations, which
#' when multiplied by (applied to) [layer-like] objects, those [layer-like] objects are distributed
#' over the [operation]s (i.e. copied).
#'
#' @param x,... [operation]s
#' @param e1 an [operation] or [numeric()]
#' @param e2 an [operation] or [numeric()]
#' @param na.rm ignored
#'
#' @details
#' Addition of \pkg{ggblend} [operation]s depends on the types of
#' objects being summed:
#'
#' - If you add an [operation] to an [operation], they are merged into
#'   a single [operation] that copies input [layer-like] objects, one for each [operation].
#' - If you add an [operation] to a [numeric()] *n*, it is equivalent to
#'   adding `*` [nop()]s to that [operation].
#'
#' @examples
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
#' @name operation_sum
#' @aliases operation_sum-class
#' @export
setClass("operation_sum", contains = c("list", "operation"))
