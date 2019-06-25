#' Try running a function and get a non-error response n times
#'
#' @param x A function.
#' @param n Number of times to try.
#' @export
#' @examples
try_try_try <- function(x, n = 3L) {
  response <- "failed"
  attempt <- 1
  while (response == "failed" && attempt <= n) {
    print(sprintf("attempt: %s", attempt))
    attempt <- attempt + 1
    try({ response <- x }, silent = TRUE)
  }
  response
}
