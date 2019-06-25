#' Move provided suffixes to separate column (and remove suffix from name column).
#'
#' @param df A data.frame or data.table with a suffix containing name column.
#' @param name_field Name of the name column/field.
#' @param suffix_list A list of suffixes to move.
#' @param return_type Type of data object to return, "dt" (data.table) or "df" (data.frame). Default "dt".
#' @import data.table
#' @export
#' @examples
#' names <- data.frame(name = c("Bob, Jr", "Dallas III", "Willy"))
#' names <- name.move_suffix(names, "name", list("Jr", "III"))
#' names
#'    name suffix
#' 1  Bob,     Jr
#' 2  Dallas    III
#' 3  Willy   <NA>
name.move_suffix <- function(df, name_field, suffix_list, return_type = "dt") {
    dt <- data.table::setDT(df)
    lapply(suffix_list, function(sfx) {
        field <- dt[, get(name_field)]
        dt[
            grepl(paste0(" ", sfx), field) == TRUE, suffix := sfx
            ][
                , eval(name_field):=sub(paste0(" ", sfx), "", field)
                ]
    })
    ifelse(return_type == "df", d <- data.table::setDF(dt), d <- dt)
    d
}

#' Standardize column names
#' Make lower case R compliant names that use an underscore rather than a dot
#' and remove apostrophes. Multiple underscores are reduced to one.
#' @param name_list List of column names.
#' @param fix_camel Change camel case to underscores before processing.
#' @export
#' @examples
#' standard_col_names(c("first.name", "LastName"))
#' [1] "first_name" "lastname"
#' name.standard_col_names(c("first.name", "LastName"), fix_camel = T)
#' [1] "first_name" "last_name"
name.standard_col_names <- function(name_list = c("first.name", "LastName"), fix_camel = FALSE) {
    o <- name_list
    ## remove camel case
    if (fix_camel == TRUE) o <- fix_camel_case(o)
    ## standardize
    o <- tolower(o)
    o <- gsub("'", "", o)
    o <- gsub("[[:punct:] ]", "_", o)
    o <- gsub("_+", "_", o)
    o <- make.unique(o, sep = "_")
    o
}


