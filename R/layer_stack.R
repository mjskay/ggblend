new_layer_stack = function(list) {
  new("layer_stack", .Data = list)
}

#' @export
layer_stack = function(...) {
  new_layer_stack(list(...))
}


# predicates --------------------------------------------------------------

#' @export
is_layer_stack = function(x) {
  inherits(x, "layer_stack")
}


# type conversion ---------------------------------------------------------

#' @export
as_layer_stack = function(x) {
  UseMethod("as_layer_stack")
}

#' @export
as_layer_stack.list = function(x) {
  if (is_layer_stack(x)) {
    return(x)
  }
  x = as.list(x)
  if (!all(vapply(x, inherits, what = "Layer", logical(1)))) {
    stop0("All objects in a layer_stack must be ggplot2 Layers")
  }
  new_layer_stack(x)
}

#' @export
as_layer_stack.Layer = function(x) {
  new_layer_stack(list(x))
}


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "layer_stack"), function(object) {
  cat("<layer_stack>:\n")
  print(object@.Data)
})
