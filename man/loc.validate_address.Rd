% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location.R
\name{loc.validate_address}
\alias{loc.validate_address}
\title{Validate address with USPS}
\usage{
loc.validate_address(address_1 = "Suite 2910",
  address_2 = "1100 Wilson Blvd", city = "Arlington", state = "VA",
  zip5 = "22209", zip4 = "", full_info = TRUE,
  usps_userid = Sys.getenv("USPS_WEBAPI_USERNAME"))
}
\value{
list
}
\description{
Validate address with USPS
}
