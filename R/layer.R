

# type predicates ---------------------------------------------------------

is_layer_instance = function(x) {
  inherits(x, "LayerInstance")
}

is_layer = function(x) {
  inherits(x, c("LayerInstance", "layer_list")) ||
    (is.list(x) && all(vapply(x, is_layer, logical(1))))
}


# type conversion ---------------------------------------------------------

#' @export
as_layer = function(x) {
  UseMethod("as_layer")
}

#' @export
as_layer.default = function(x) {
  stop0("Cannot convert object of type ", deparse1(class(x)), " to a Layer")
}

#' @export
as_layer.Layer = function(x) {
  x
}

#' @export
as_layer.list = function(x) {
  as_layer_list(x)
}

#' @export
as_layer.layer_list = function(x) x

#' @export
as_layer.layer_stack = function(x) x


# layer manipulation ------------------------------------------------------

#' When when manipulating layers (e.g. mapping functions over objects to return
#' lists of layers), use this function to simplify the result, ensure it is ne
#' of the layer types, and (optionally) that it is of a specific layer type
#' (pure, layer, stack, or list)
#' @param layers layer(s) to simplify
#' @param prototype object whose type determines the output type if
#'  x is a list
#' @noRd
setGeneric("simplify_layers", function(layers, prototype) {
  layers
})

setMethod("simplify_layers", signature(prototype = "layer_stack"), function(layers, prototype) {
  as_layer_stack(layers)
})

setMethod("simplify_layers", signature(layers = "list", prototype = "layer_list"), function(layers, prototype) {
  if (length(layers) == 1) {
    layers[[1]]
  } else {
    as_layer_list(layers)
  }
})

#' Apply a function over a layer, returning an object of the same type of layer
#' (pure layer, layer list, or layer stack)
#' @noRd
layer_apply = function(.x, .f, ...) {
  UseMethod("layer_apply")
}

#' @export
layer_apply.LayerInstance = function(.x, .f, ...) {
  .f(.x, ...)
}

#' @export
layer_apply.list = function(.x, .f, ...) {
  simplify_layers(lapply(.x, .f, ...), prototype = .x)
}
