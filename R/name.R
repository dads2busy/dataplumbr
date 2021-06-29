#' Move provided suffixes to separate column (and remove suffix from name column).
#'
#' @param x A data.frame, data.table or tbl with a suffix containing name column.
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
name.move_suffix <- function(x, name_field, suffix_list) {
    UseMethod("name.move_suffix")
}
name.move_suffix.data.frame <- function(x, name_field, suffix_list) {
    dt <- name.apply_suffix_list(x, name_field, suffix_list)
    return(data.table::setDF(dt))
}
name.move_suffix.data.table <- function(x, name_field, suffix_list) {
    dt <- name.apply_suffix_list(x, name_field, suffix_list)
    return(dt)
}
name.move_suffix.tbl_df <- function(x, name_field, suffix_list) {
    dt <- name.apply_suffix_list(x, name_field, suffix_list)
    return(dplyr::as.tbl(dt))
}
name.apply_suffix_list <- function(x, name_field, suffix_list) {
    dt <- data.table::as.data.table(x)
    lapply(suffix_list, function(sfx) {
        field <- dt[, get(name_field)]
        dt[
            grepl(paste0(" ", sfx), field) == TRUE, suffix := sfx
            ][
                , eval(name_field):=sub(paste0(" ", sfx), "", field)
                ]
    })
    return(dt)
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
name.standard_col_names <- function(name_list = c("first.name", "LastName"), fix_acronyms = TRUE, fix_camel = FALSE) {
    o <- name_list
    ## change acronyms to title case
    if (fix_acronyms == TRUE) o <- name.fix_acronyms(o)
    ## remove camel case
    if (fix_camel == TRUE) o <- name.fix_camel_case(o)
    ## standardize
    o <- tolower(o)
    o <- gsub("%", "pct", o)
    o <- gsub("'", "", o)
    o <- gsub("[[:punct:] ]", "_", o)
    o <- gsub("_+", "_", o)
    o <- make.unique(o, sep = "_")
    o
}

#' Convert camel case to underscore separated words
#' @param name_list List of camel case strings to convert.
#' @export
#' @examples
#' name.fix_camel_case(c("I'mACamel", "NoYouAreNot"))
#' [1] "I'm_A_Camel"    "No_You_Are_Not"
name.fix_camel_case <- function (name_list = c("I'mACamel"))
{
    o <- gsub("([[:upper:]])([[:lower:][:upper:]][[:lower:]])",
              "_\\1\\2", name_list)
    o <- gsub("(^_)", "", o)
    o <- tolower(o)
    o
}


#' Change acronyms (multiple capital letters) to title case
#' @param str List of colnames to convert.
#' @export
#' @examples
#' name.fix_acronyms(c("FIPSCode", "NoAcronymHere", "Multi-APNFlag"))
#' [1] "FipsCode"      "NoAcronymHere" "Multi-ApnFlag"
name.fix_acronyms <- function (str = c("FIPSCode", "NoAcronymHere", "Multi-APNFlag"))
{
    out <- list()
    for(i in 1:length(str)) {
        acro <- stringr::str_match(str[i], "([:upper:][:upper:]([:upper:]+)?)[:lower:]")[2]
        if (!is.na(acro)) {
            acro2 <- str_sub(acro, 1, nchar(acro) - 1)
            acro3 <- str_to_title(acro2)
            out <- c(out, str_replace(str[i], acro2, acro3))
        } else {
            out <- c(out, str[i])
        }
    }
    unlist(out)
}


