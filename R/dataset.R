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

#' Split hyphenated strings in column into two columns
#'
#' @param df data.frame or data.table.
#' @param name_field name of column containing hyphenated values.
#' @param return_type. Optional. Return data.table "dt" or data.frame "df". Default "dt".
#' @import data.table
#' @export
#' @examples
#' df <- data.frame(first_name = c("Devante"), last_name = c("Smith-Pelly"))
#' dt <- split_hyphenated(df, "last_name")
#' dt
#'    first_name last_name last_name_left last_name_right
#' 1:    Devante     Smith          Smith           Pelly
split_hyphenated <- function(df, name_field, return_type = "dt") {
    dt <- data.table::setDT(df)
    field <- dt[, get(name_field)]
    groups <- stringr::str_match(field, "(.*)-(.*)")
    dt[, paste0(name_field, "_left") := groups[,2]]
    dt[, paste0(name_field, "_right") := groups[,3]]
    #browser()
    dt[!is.na(get(paste0(name_field, "_left"))), eval(name_field) := get(paste0(name_field, "_left"))]
    ifelse(return_type == "df", d <- data.table::setDF(dt), d <- dt)
    d
}
