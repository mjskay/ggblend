#' @export
nop = function() {
  new("Nop")
}


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "Nop"), function(operation, layers) {
  simplify_layers(layers, prototype = layers)
})


# operation multiplication ------------------------------------------------

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "Operation", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "OperationSum", e2 = "Nop"), function(e1, e2) e1)

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "Operation"), function(e1, e2) e2)

#' @export
setMethod("*", signature(e1 = "Nop", e2 = "OperationSum"), function(e1, e2) e2)


# printing ----------------------------------------------------------------

#' @export
setMethod("format", signature(x = "Nop"), function(x, ...) {
  "1"
})
