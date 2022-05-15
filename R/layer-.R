
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
as_layer.LayerInstance = function(x) {
  x
}

#' @export
as_layer.list = function(x) {
  as_layer_list(x)
}

#' @export
as_layer.layer_list = function(x) x

#' @export
as_layer.layer_group = function(x) x


# layer manipulation ------------------------------------------------------

#' Apply a function over a layer, returning an object of the same type of layer
#' (pure layer, layer list, or layer group)
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
  rapply(.x, .f, classes = "LayerInstance", how = "replace")
}
