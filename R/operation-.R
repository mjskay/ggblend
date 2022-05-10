#' @export
setClass("Operation")


# operation application ---------------------------------------------------

setGeneric("apply_operation", function(operation, layers) {
  stop0("Unimplemented layer operation")
})

#' @export
setMethod("*", signature(e1 = "Operation"), function(e1, e2) {
  apply_operation(e1, as_layer_list(e2))
})

#' @export
setMethod("*", signature(e2 = "Operation"), function(e1, e2) {
  apply_operation(e2, as_layer_list(e1))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "Operation"), function(object) {
  arg_names = slotNames(object)
  args = attributes(object)[arg_names]
  args_string = paste0(names(args), " = ", vapply(args, deparse1, character(1)), collapse = ", ", recycle0 = TRUE)
  cat0(tolower(class(object)), "(", args_string, ")\n")
  invisible(object)
})
