#' Lists of layer-like objects
#'
#' A list of [layer-like] objects, which can be used in layer [operation]s
#' (through function application or multiplication) or added to a [ggplot2()]
#' object.
#'
#' @param x,... [layer-like] objects
#' @param object,e1,e2 [layer_list()]s
#'
#' @details
#' For the most part, users of \pkg{ggblend} need not worry about this class.
#' It is used internally to simplify multiple dispatch on binary operators, as
#' the alternative ([list()]s of [ggplot2::layer()]s) is more cumbersome.
#' \pkg{ggblend} converts input lists to this format as needed.
#'
#' @returns
#' An object of class `"layer_list"`.
#'
#' @examples
#' library(ggplot2)
#'
#' # layer_list()s act just like list()s of layer()s in that they can
#' # be added to ggplot() objects
#' data.frame(x = 1:10) |>
#'   ggplot(aes(x, x)) +
#'   layer_list(
#'     geom_line(),
#'     geom_point()
#'   )
#'
#' @name layer_list
#' @aliases layer_list-class
#' @export
setClass("layer_list", contains = "list")
