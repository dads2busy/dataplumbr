#' Verify directory exists (if not, create)
#'
#' @param full_path Path to directory.
#' @export
#' @examples
#' dir.verify_create("~/missingfolder")
#' Creates folder named "missingfolder" in your home directory
dir.verify_create <- function(full_path, recursive = TRUE) {
    if (!file.exists(full_path)) {
        dir.create(path = full_path, recursive = recursive)
        print(sprintf("%s created", full_path))
    }
    else (warning(sprintf("%s already exists", full_path)))
}
