#' Unzip File to Temp Directory and Return Directory Path
#'
#' @param zipfilepath Path to the zip file.
#' @export
#' @examples
#' file.unzip2temp("data/CENSUS/cb/cb_2016_01_bg_500k.zip")
#' [1] "/tmp/Rtmp8lE8Pj/"
file.unzip2temp <- function(zipfilepath) {
    tempdir <- tempdir()
    print(unzip(zipfilepath, exdir = tempdir))
    tempdir
}

#' Download and Unzip File to Temp Directory and Return File Paths
#'
#' @param url url to download the zip file.
#' @export
file.download_unzip2temp <- function(url) {
    # browser()
    tempdir <- tempdir()
    unlink(list.files(tempdir, full.names = T), recursive = T)
    tempfile <- file.path(tempdir, basename(url))
    download.file(url, tempfile)
    utils::unzip(tempfile, exdir = tempdir)
    file.remove(tempfile)
    list.files(tempdir, full.names = T)
}

#' Download and Un gzip File to Temp Directory and Return File Paths
#'
#' @param url url to download the zip file.
#' @export
file.download_ungz2temp <- function(url) {
    tempdir <- tempdir()
    file.remove(list.files(tempdir, full.names = T))
    tempfile <- file.path(tempdir, basename(url))
    download.file(url, tempfile)
    R.utils::gunzip(tempfile)
    list.files(tempdir, full.names = T)
}

#' Find By File Type in Directory
#'
#' @param type Type of file file extension without a period, e.g. "zip", "shp".
#' @param first_only Only return first found. default FALSE.
#' @param full_path Return file name with full path. defualt FASLE.
#' @export
#' @examples
#' file.findByType(tmpdir, "shp", first_only = TRUE)
#' [1] "some_shape_file.shp"
file.findByType <- function(directory, type = "shp", first_only = FALSE, full_path = FALSE) {
    if (first_only == FALSE) list.files(directory, pattern = paste0(".*\\.", type, "$"), full.names = full_path)
    else list.files(directory, pattern = paste0(".*\\.", type, "$"), full.names = full_path)[[1]]
}


