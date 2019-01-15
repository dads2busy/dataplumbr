#' Verify Directory Exists (if not, create)
#'
#' @param full_path Path to directory.
#' @export
#' @examples
#' findByType(tmpdir, "shp", first_only = TRUE)
#' [1] "some_shape_file.shp"
dir.verify <- function(full_path, recursive = TRUE) {
    if (!file.exists(full_path)) dir.create(path = full_path, recursive = recursive)
    else (warning(sprintf("%s already exists", full_path)))
}
