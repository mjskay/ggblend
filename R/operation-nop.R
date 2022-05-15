new_nop = function() {
  new("nop")
}

#' @rdname nop
#' @export
nop = make_operation("nop", new_nop)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "nop"), function(operation, layers) {
  simplify_layers(layers, prototype = layers)
})


# operation multiplication ------------------------------------------------

#' @export
setMethod("*", signature(e1 = "nop", e2 = "nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "operation", e2 = "nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "operation_sum", e2 = "nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "nop", e2 = "operation"), function(e1, e2) e2)

#' @export
setMethod("*", signature(e1 = "nop", e2 = "operation_sum"), function(e1, e2) e2)


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "nop"), function(x, ...) {
  "1"
})
