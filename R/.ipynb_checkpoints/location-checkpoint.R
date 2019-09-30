#' Get census block, county, state, and market area information based on latitude/longitude input.
#'
#' @param place_id some unique identifier for the lat lon
#' @param lat the latitude
#' @param lon the longitude
#' @return data.frame
#' @export
#' @examples
#' loc.lat_lon2geo_areas("VTRC", lat=38.880807, lon=-77.11577)
loc.lat_lon2geo_areas<- function(place_id = "VTRC", lat = 38.880807, lon = -77.11577) {
    if (length(place_id) > 1) {stop('you supplied multiple values for place_id, did you mean to use FCClocations2FIPS?')}
    if (length(lat) > 1) {stop('you supplied multiple values for lat, did you mean to use FCClocations2FIPS?')}
    if (length(lon) > 1) {stop('you supplied multiple values for lon, did you mean to use FCClocations2FIPS?')}

    url <- sprintf('https://geo.fcc.gov/api/census/area?lat=%s&lon=%s&format=json', lat, lon)
    res <- jsonlite::fromJSON(url)
    res$results
}

#' Get census block, county, state, and market area information based on multiple latitude/longitude inputs.
#'
#' @param place_idCol vector of unique identifiers
#' @param latCol vector of latitudes
#' @param lonCol vector of longitudes
#' @return data.frame
#' @export
#' @examples
#' loc.lats_lons2geo_areas(place_idCol = c("VTRC", "VT-NVC"),
#'                   latCol = c(38.880807, 38.8968325),
#'                   lonCol = c(-77.11577, -77.1894815))
loc.lats_lons2geo_areas <- function(place_idCol = c("VTRC", "VT-NVC"), latCol = c(38.880807, 38.8968325), lonCol = c(-77.11577, -77.1894815)) {
    as.data.frame(t(mapply(loc.lat_lon2geo_areas, place_idCol, latCol, lonCol)))
}

# Validate Address using USPS API
#https://secure.shippingapis.com/ShippingAPI.dll?API=Verify&XML=%3CAddressValidateRequest%20USERID=%22155UVA006393%22%3E%3CAddress%3E%3CAddress1%3E%3C/Address1%3E%3CAddress2%3E4504%2018th%20street%20N%3C/Address2%3E%3CCity%3E%3C/City%3E%3CState%3EVA%3C/State%3E%3CZip5%3E22207%3C/Zip5%3E%3CZip4%3E%3C/Zip4%3E%3C/Address%3E%3C/AddressValidateRequest%3E
