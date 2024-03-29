stop0 = function(...) {
  stop(..., call. = FALSE)
}

warning0 = function(...) {
  warning(..., call. = FALSE)
}

cat0 = function(...) {
  cat(..., sep = "")
}

format_name_value_pairs = function(x, defaults = list()) {
  # remove values that are set to their defaults or to NULL
  x = unclass(x)
  x[as.logical(mapply(identical, x, defaults[names(x)]))] = NULL

  if (length(x) > 0) {
    paste0(
      names(x), " = ",
      vapply(x, function(x) if (is.character(x)) deparse1(x) else format(x), character(1)),
      collapse = ", "
    )
  }
}

bullet = function(x) {
  paste0(strwrap(paste0("- ", x), exdent = 3, indent = 1), collapse = "\n")
}
