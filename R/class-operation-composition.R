#' Layer operation composition
#'
#' [operation]s can be composed together to form chains of operations, which
#' when multiplied by (applied to) [Layer]s, return modified [Layer]s. In
#' contrast to [operation_product]s, compositions of operations are not
#' distributive over sums of [operation]s or [layer]s.
#'
#' @details
#' Composition of \pkg{ggblend} [operation]s depend on the types of
#' objects being multiplied:
#'
#' - If you compose an [operation] with an [operation], they are merged into
#'   a single [operation] that applies each [operation] in sequence, without
#'   distributing over layers.
#' - If you compose an [operation] with a [Layer], that operation is applied
#'   to the layer, returning a new [Layer]. The operation is applied to the
#'   layer as a whole, not any sub-parts (e.g. sub-layers or graphical objects).
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # composing operations together chains them
#' adjust(color = "red") |> adjust(size = 2)
#'
#' # unlike multiplication, composition does not follow the distributive law
#' op = (adjust(aes(y = 11 -x), color = "skyblue") + 1) |> (adjust(color = "white", size = 4) + 1)
#' op
#'
#' # multiplication by a geom returns a modified version of that geom
#' data.frame(x = 1:10) |>
#'   ggplot(aes(x = x, y = x)) +
#'   geom_line(size = 2) * op
#' @name operation_composition
#' @aliases operation_composition-class
#' @export
setClass("operation_composition", representation(operation1 = "operation", operation2 = "operation"), contains = c("operation"))
