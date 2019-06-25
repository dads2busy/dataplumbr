#' Remove rows that are completely NA
#'
#' @param df data.frame or data.table.
#' @import data.table
#' @export
#' @examples
#' na_df <- data.frame(col1 = c("Hola", NA), col2 = c("Chicka", NA))
#' na_df <- set.rm_all_NA_rows(na_df)
#' na_df
#'    col1   col2
#' 1: Hola Chicka
#' 2:   NA     NA
set.rm_all_NA_rows <- function(df) {
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
#' dt <- set.split_hyphenated(df, "last_name")
#' dt
#'    first_name last_name last_name_left last_name_right
#' 1:    Devante     Smith          Smith           Pelly
set.split_hyphenated <- function(df, name_field, return_type = "dt") {
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

#' Deduplication chooser
#' Majority rules, ties goes to lowest value after ordering, so most recent if a date or time.
#'
#' @param df Data.frame or data.table
#'
set.dedup_choice <- function(df) {
    dt <- data.table::setDT(df)
    for (j in colnames(dt)) {
        data.table::set(dt, j = j, value = dt[get(j) != "", .N, j][order(-N)][, ..j][1])
    }
    dt[1]
}

set.dedup_choice_by_key <- function(df, key = "uid") {
    if (exists("out_dt") == TRUE) rm(out_dt, envir = globalenv())

    dt <- data.table::setDT(df)
    unique_keys <- unique(dt[, get(key)])
    key_cnt <- length(unique_keys)
    pb <- progress::progress_bar$new(format = "[:bar] :current/:total :percent eta: :eta", total = key_cnt)

    for (k in unique_keys) {
        pb$tick()
        g <- dt[get(key)==k]
        r <- dedup_choice(g)
        if (exists("out_dt") == FALSE) out_dt <- r else out_dt <- rbindlist(list(out_dt, r))
    }

    out_dt
}

#' Create new data.table rows for each combination of first and last name
#'
#' @param single_row_dt Data.table with one row/record.
#' @param fname_col First name column name.
#' @param lname_col Last name column name.
#' @param delims Delimters to split string by.
#' @export
#' @examples
#' dt <-
#' data.table(
#'   id = c(1, 2),
#'   fname = c("Aaron David", "Blaine-Myers"),
#'   lname = c("Schroeder-Dingdong", "Dingbat Tumbleweed"),
#'   dob = "11/13/1968"
#' )
#' final_dt <- dt[, set.new_row_per_string_item(.SD, fname_col = "fname", lname_col = "lname"), id]
set.new_row_per_string_item <- function(single_row_dt, fname_col, lname_col, delims = "[-, ]") {
    #browser()
    dt <- data.table::setDT(single_row_dt)
    #print(dt[, 1:3])
    fnames <- string2vector(dt[, get(fname_col)], delimeter = delims)
    if (is.na(fnames[1]) == TRUE) fnames <- ""
    lnames <- string2vector(dt[, get(lname_col)], delimeter = delims)
    if (is.na(lnames[1]) == TRUE) lnames <- ""
    for (fn in fnames) {
        for (ln in lnames) {
            new_dt <- data.table::copy(dt)
            new_dt[, eval(fname_col) := fn]
            new_dt[, eval(lname_col) := ln]

            if (exists("out_dt") == TRUE) {
                out_dt <- data.table::rbindlist(list(out_dt, new_dt))
            }
            else{
                out_dt <- new_dt
            }
        }
    }
    out_dt
}


