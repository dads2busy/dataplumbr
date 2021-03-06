% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir.R
\name{dir.verify_create}
\alias{dir.verify_create}
\title{Verify directory exists (if not, create)}
\usage{
dir.verify_create(full_path, recursive = TRUE)
}
\arguments{
\item{full_path}{Path to directory.}
}
\description{
Verify directory exists (if not, create)
}
\examples{
dir.verify_create("~/missingfolder")
Creates folder named "missingfolder" in your home directory
}
