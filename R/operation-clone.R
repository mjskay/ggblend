#' @export
clone_above = function(mapping = aes(), ...) {
  1 + adjust(mapping = mapping, ...)
}

#' @export
clone_below = function(mapping = aes(), ...) {
  adjust(mapping = mapping, ...) + 1
}
