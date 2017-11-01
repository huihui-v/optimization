call_gradient <- function () {
  gradient_called <<- gradient_called + 1
}

call_function <- function () {
  function_called <<- function_called + 1
}

call_Hesse <- function () {
  Hesse_called <<- Hesse_called + 1
}