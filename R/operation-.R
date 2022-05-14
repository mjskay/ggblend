
# type conversion ---------------------------------------------------------

setAs("numeric", "Operation", function(from) {
  from * nop()
})


# operation application ---------------------------------------------------

setGeneric("apply_operation", function(operation, layers) {
  stop0("Unimplemented layer operation")
})

#' @export
setMethod("*", signature(e1 = "Operation"), function(e1, e2) {
  apply_operation(e1, as_layer(e2))
})

#' @export
setMethod("*", signature(e2 = "Operation"), function(e1, e2) {
  apply_operation(e2, as_layer(e1))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "Operation"), function(object) {
  cat0("<Operation>: ", format(object), "\n")
  invisible(object)
})

#' @export
setMethod("format", signature(x = "Operation"), function(x, ...) {
  arg_names = setdiff(slotNames(x), ".Data")
  args = attributes(x)[arg_names]
  args_string = format_name_value_pairs(args)
  paste0(tolower(class(x)), "(", args_string, ")")
})

