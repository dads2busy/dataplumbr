#' Remove rows that are completely NA
#'
#' @param df data.frame or data.table.
#' @import data.table
#' @export
#' @examples
#' na_df <- data.frame(col1 = c("Hola", NA), col2 = c("Chicka", NA))
#' na_df <- rm_all_NA_rows(na_df)
#' na_df
#'    col1   col2
#' 1: Hola Chicka
#' 2:   NA     NA
rm_all_NA_rows <- function(df) {
    dt <- data.table::setDT(df)
    na_rows <- apply(dt, 1, function(x) all(is.na(x)))
    dt <- dt[!na_rows,]

    # set records changed count attribute
    rm <- sum(na_rows, na.rm=TRUE)
    attr(dt, "count") <- rm

    # return data.frame
    data.table::setDF(dt)
}
