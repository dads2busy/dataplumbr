#' Check if Social Secutiy Number is Valid.
#'
#' @param ssn Social Security Number to check.
#' @import stringr
#' @export
#' @examples
#' id.valid_ssn("123-45-6789")
#' [1] FALSE
id.valid_ssn <- function(ssn) {
    ssn <- remove_non_alphanum(ssn)

    re_fmt <- "^(123456789|078051120|219099999|9\\d{8}|000\\d{6}|\\d{3}00\\d{4}|\\d{5}0000|666\\d{6})$"
    re_lng <- "^(\\d{9})$"

    inv_fmt <- stringr::str_detect(ssn, re_fmt)
    inv_lng <- stringr::str_detect(ssn, re_lng)

    if (inv_fmt == T | inv_lng == F) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' Add a valid_ssn column to a data.frame or data.table.
#'
#' @param df data.frame or data.table with an ssn column.
#' @param ssn_field Name of the ssn column/field.
#' @import data.table
#' @export
#' @examples
#' ssn_df <- data.frame(ssn = c("123-45-6789", "368-96-8955", "999998888", "287-65-4321"))
#' ssn_df <- id.valid_ssn_col(ssn_df, "ssn")
#' ssn_df
#' ssn ssn_valid
#' 1 123-45-6789    FALSE
#' 2 367-94-8940     TRUE
#' 3   999998888    FALSE
#' 4 287-65-4321     TRUE
id.add_valid_ssn_col <- function(df, ssn_field) {
    dt <- data.table::setDT(df)
    dt[, paste0(ssn_field, "_valid") := valid_ssn(get(ssn_field)), ssn_field]
    data.table::setDF(dt)
}
