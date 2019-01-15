#' Assign date to 1900s or 2000s correctly.
#'
#' # fix year by assigning a date (date_ymd) above the cutoff date (cut_date_ymd) to the 1900s
#' or below to the 2000s.
#'
#' @param date_ymd Date to check. YMD format ("2020-03-26").
#' @param cut_date_ymd Optional. Date to check against. YMD format ("2020-03-26"). Defaults to current system date.
#' @import lubridate
#' @import data.table
#' @export
#' @examples
#' fix_century("2049-11-30")
#' [1] "1949-11-30"
fix_century <- function(date_ymd = lubridate::ymd("2020-03-26"), cut_date_ymd = lubridate::ymd(Sys.Date())) {
    date = tryCatch({
        as.Date(date_ymd)
    }, error = function(e) {
        stop("Not a Date")
    })

    cut_date = tryCatch({
        as.Date(cut_date_ymd)
    }, error = function(e) {
        stop("Not a Date")
    })

    yr <- lubridate::year(date) %% 100
    lubridate::year(date) <- ifelse(lubridate::year(date) > lubridate::year(cut_date), 1900 + yr, lubridate::year(date))
    date
}

#' Create separate year, month, day columns from ymd or mdy formatted date.
#'
#' @param df data.frame or data.table with a date field.
#' @param date_field Name of the date field/column.
#' @param date_format Optional. Date field format. Either "ymd" (Year-Month-Day)
#' or "mdy" (Month-Day-Year). Defaults to "ymd".
#' @import lubridate
#' @import data.table
#' @export
#' @examples
#' df_with_dob_column <- data.frame(dob = c("11/13/1968"))
#' df_with_parsed_dob <- parse_dob_to_cols(df_with_dob_column, date_field = "dob", date_format = "mdy")
#' df_with_parsed_dob
#'          dob birth_yr birth_mo birth_dy
#' 1 11/13/1968     1968       11       13
parse_dob_to_cols <- function(df, date_field, date_format = "ymd") {
    dt <- data.table::setDT(df)
    if(date_format == "ymd") {
        dt[, birth_yr := lubridate::year(lubridate::ymd(dt[, get(date_field)]))]
        dt[, birth_mo := lubridate::month(lubridate::ymd(dt[, get(date_field)]))]
        dt[, birth_dy := lubridate::day(lubridate::ymd(dt[, get(date_field)]))]
    } else if (date_format == "mdy") {
        dt[, birth_yr := lubridate::year(lubridate::mdy(dt[, get(date_field)]))]
        dt[, birth_mo := lubridate::month(lubridate::mdy(dt[, get(date_field)]))]
        dt[, birth_dy := lubridate::day(lubridate::mdy(dt[, get(date_field)]))]
    }
    data.table::setDF(dt)
}
