#' Remove non-alphanumeric characters from a string.
#'
#' @param string A character string.
#' @export
#' @examples
#' remove_non_alphanum("222-44-6666")
#' [1] "222446666"
remove_non_alphanum <- function(string) {
    gsub("[^[:alnum:] ]", "", string)
}

#' Convert string to character vector
#'
#' @param string A character string.
#' @param delimeter Optional. Default ",".
#' @export
#' @examples
#' remove_non_alphanum("222-44-6666")
#' [1] "222446666"
string2vector <- function(string, delimeter=",") {
    unlist(strsplit(string, delimeter))
}

#' Return everything before the first occurence of provided separator.
#'
#' @param string A character string.
#' @param separator Optional. Default ",".
#' @export
#' @examples
#' get_before_separator("I am smart enough, good looking enough, and gosh darn it, people like me.")
#' [1] "I am smart enough"
get_before_separator <-  function(string = "", separator = ",") {
    trimws(substr(string, 1, regexpr(separator, string) - 1))
}

#' Return everything between first occurence of first separator and first occurence
#' of second separator after the first separator.
#'
#' @param string A character string.
#' @param first_separator Optional. Default ",".
#' @param second_separator Optional. Default ",".
#' @export
#' @examples
#' str <- "I am smart enough, good looking enough, and gosh darn it; people like me."
#' get_between_separators(str)
#' [1] "good looking enough"
#' get_between_separators(str, second_separator = ";")
#' [1] "good looking enough, and gosh darn it"
get_between_separators <- function(string = "", first_separator = ",", second_separator = ",") {
    idx_fst_sep <- regexpr(first_separator, string) + 1
    idx_scd_sep <- regexpr(second_separator, substr(string, idx_fst_sep, nchar(string))) + idx_fst_sep - 2
    trimws(substr(string, idx_fst_sep, idx_scd_sep))
}
