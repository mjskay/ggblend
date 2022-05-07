
# constructuor ------------------------------------------------------------

#' An unevaluated call to stack_blends()
#' @param ... ggblend objects
#' @noRd
new_ggblend_stack = function(...) {
  # check all objects are blends
  blends = lapply(list(...), as_ggblend)

  # expand blend stacks into their constituent parts
  blends = lapply(blends, function(blend) {
    if (inherits(blend, "ggblend_stack")) {
      blend
    } else {
      list(blend)
    }
  })

  # TODO: could make this a real S4 class, but in the meantime this works
  # as a hack just to get dispatch working correctly on `*` when ggplot objects
  asS4(structure(unlist(blends, recursive = FALSE, use.names = FALSE), class = c("ggblend_stack", "ggblend")))
}

setOldClass(c("ggblend_stack", "ggblend"))


# operators ---------------------------------------------------------------

#' @rdname times-ggblend
#' @export
setMethod("*", signature(e1 = "ggblend_stack"), function(e1, e2) {
  do.call(stack_blends, c(list(x = e2), e1))
})

#' @rdname times-ggblend
#' @export
setMethod("*", signature(e2 = "ggblend_stack"), function(e1, e2) {
  do.call(stack_blends, c(list(x = e1), e2))
})


# printing ----------------------------------------------------------------

#' @rdname print.ggblend
#' @export
print.ggblend_stack = function(x, ...) {
  cat0("<stack_blends>( ,\n")
  for (blend in x) {
    cat0("  ")
    print(blend)
  }
  cat0(")\n")
  invisible(x)
}

#' @rdname print.ggblend
#' @export
setMethod("show", signature(object = "ggblend_stack"), function(object) print.ggblend_stack(object))
