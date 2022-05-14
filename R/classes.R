
# Operations --------------------------------------------------------------

setClass("Operation")

#' @export
setClass("Adjust", representation(mapping = "ANY", params = "list"), contains = "Operation")

#' @export
setClass("Nop", contains = "Operation")


# Operation sums and products ---------------------------------------------

#' @export
setClass("OperationSum", contains = c("list", "Operation"))

#' @export
setClass("OperationProduct", contains = c("list", "Operation"))


# Layers -------------------------------------------------------

#' @export
setClass("LayerList", contains = "list")

#' @export
setClass("LayerStack", contains = "list")
