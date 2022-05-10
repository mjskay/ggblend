#' @export
setClass("Clone", contains = "Operation")

#' @export
clone = function() {
  new("Clone")
}

#' @export
setMethod("apply_operation", signature(operation = "Clone"), function(operation, layers) {
  simplify_layer_list(layers)
})
