#' Get census block, county, state, and market area information based on latitude/longitude input
#' from the FCC API.
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

#' Validate address with U.S. Postal Service (USPS) Web Tools
#'
#' @param address_1 Optional. Secondary Delivery Address. May contain secondary unit designator, such as APT or SUITE.
#' @param address_2 Optional. Primary Delivery Address.
#' @param city Optional. City name of the destination address. Maximum characters allowed: 15.
#' @param state Optional. Two-character state code of the destination address. Maximum characters allowed: 2.
#' @param zip5 Optional. Destination 5-digit ZIP Code.StringMust be 5-digits. Numeric values (0-9) only.
#' @param zip4 Optional. Destination 4-digit +ZIP Code. Numeric values (0-9) only.
#' @param full_info Required. Logical value used to flag return of all response fields.
#' @return list
#' @export
loc.validate_address <- function(address_1 = "",
                                 address_2 = "1100 Wilson Blvd",
                                 city = "Arlington",
                                 state = "VA",
                                 zip5 = "",
                                 zip4 = "",
                                 full_info = TRUE,
                                 usps_userid = Sys.getenv("USPS_WEBAPI_USERNAME")) {
    if (full_info == TRUE) rev <- 1 else rev <- 0
    url <- paste0("https://secure.shippingapis.com/ShippingAPI.dll?API=Verify&XML=<AddressValidateRequest USERID=\"", usps_userid, "\">
        <Revision>", rev,"</Revision>
        <Address>
            <Address1>", address_1,"</Address1>
            <Address2>", address_2,"</Address2>
            <City>", city,"</City>
            <State>", state,"</State>
            <Zip5>", zip5,"</Zip5>
            <Zip4>", zip4,"</Zip4>
        </Address>
        </AddressValidateRequest>") %>%
        stringr::str_replace_all("(\n|\\s+)", " ") %>%
        utils::URLencode()
    return(xml2::as_list(xml2::read_xml(url)))
}
