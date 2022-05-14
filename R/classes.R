
# Operations --------------------------------------------------------------

#' @rdname Operation-class
#' @export
setClass("Operation")

#' @rdname adjust
#' @export
setClass("Adjust", representation(mapping = "ANY", params = "list"), contains = "Operation")

#' @rdname nop
#' @export
setClass("Nop", contains = "Operation")


# Operation sums and products ---------------------------------------------

#' @rdname OperationSum-class
#' @export
setClass("OperationSum", contains = c("list", "Operation"))

#' @rdname OperationProduct-class
#' @export
setClass("OperationProduct", contains = c("list", "Operation"))


# Layers -------------------------------------------------------

#' @export
setClass("LayerList", contains = "list")

#' @export
setClass("LayerStack", contains = "list")
