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
#' names <- move_suffix(names, "name", list("Jr", "III"))
#' names
#'    name suffix
#' 1  Bob,     Jr
#' 2  Dallas    III
#' 3  Willy   <NA>
move_suffix <- function(df, name_field, suffix_list, return_type = "dt") {
    dt <- data.table::setDT(df)
    lapply(suffix_list, function(sfx) {
        field <- dt[, get(name_field)]
        dt[
            field %like% paste0(" ", sfx),
            suffix := sfx
        ][
            ,
            eval(name_field):=sub(paste0(" ", sfx), "", field)
        ]
    })
    ifelse(return_type == "df", d <- data.table::setDF(dt), d <- dt)
    d
}
