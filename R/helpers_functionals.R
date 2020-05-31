subtract <- function(.f, .minus, ...) {
  function(...) { .f(...) - .minus }
}
