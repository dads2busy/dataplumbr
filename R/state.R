#' Checks if provided variable is null, na, or of length 0
#'
#' @param x Variable provided.
#' @export
#' @examples
#' var.is_blank("")
#' [1] TRUE
var.is_blank <- function(x) {
    return(is.null(x) |
               length(x) == 0 |
               is.na(x) |
               x == "")
}
