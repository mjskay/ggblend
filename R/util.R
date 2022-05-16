stop0 = function(...) {
  stop(..., call. = FALSE)
}

warning0 = function(...) {
  warning(..., call. = FALSE)
}

cat0 = function(...) {
  cat(..., sep = "")
}

format_name_value_pairs = function(x) {
  if (length(x) > 0) {
    paste0(
      names(x), " = ",
      vapply(x, deparse1, character(1)),
      collapse = ", "
    )
  }
}
