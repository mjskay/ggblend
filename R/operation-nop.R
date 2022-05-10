#' @export
setClass("Nop", contains = "Operation")

#' @export
nop = function() {
  new("Nop")
}

#' @export
setMethod("apply_operation", signature(operation = "Nop"), function(operation, layers) {
  simplify_layer_list(layers)
})
