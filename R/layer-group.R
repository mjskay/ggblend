new_layer_group = function(list) {
  new("layer_group", .Data = list)
}

#' @export
layer_group = function(...) {
  new_layer_group(list(...))
}


# type conversion ---------------------------------------------------------

#' @export
as_layer_group = function(x) {
  UseMethod("as_layer_group")
}

#' @export
as_layer_group.list = function(x) {
  new_layer_group(as_layer_list(x))
}

#' @export
as_layer_group.LayerInstance = function(x) {
  new_layer_group(list(x))
}


# layer concatenation -------------------------------------------------

#' @export
setMethod("+", signature(e1 = "layer_group", e2 = "layer_list"), function(e1, e2) {
  new_layer_group(c(e1, e2))
})

#' @export
setMethod("+", signature(e1 = "layer_list", e2 = "layer_group"), function(e1, e2) {
  new_layer_group(c(e1, e2))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "layer_group"), function(object) {
  cat("<layer_group>:\n")
  print(object@.Data)
})
