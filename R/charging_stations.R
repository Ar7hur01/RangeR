#' Fetch charging stations from OpenChargeMap
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @param distance_km Distance in kilometers.
#' @param api_key OpenChargeMap API Key (4f60a79e-6773-4844-ba53-9671a50b3916)
#' @return An sf object with charging station locations.
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @keywords internal
fetch_chargers_ocm <- function(lat, lon, distance_km, api_key = Sys.getenv("OCM_API_KEY")) {
  
  # Check, if key is available
  if (api_key == "") {
    warning("No OpenChargeMap API-Key found!")
    return(NULL)
  }
  # Integration of API Key and limitation to 999 stations
  url <- paste0(
    "https://api.openchargemap.io/v3/poi/?output=json",
    "&latitude=", lat,
    "&longitude=", lon,
    "&distance=", distance_km,
    "&distanceunit=KM",
    "&maxresults=999",
    "&compact=true",
    "&key=", api_key
  )
  
  # requesting the Data from URL as Json-file
  res <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    message("Error API-request: ", e$message)
    return(NULL)
  })
  
  if (is.null(res) || length(res) == 0 || !"AddressInfo" %in% names(res)) {
    return(NULL)
  }
  
  # merging lon/lat to generate coordinates
  chargers_sf <- sf::st_as_sf(
    res$AddressInfo, 
    coords = c("Longitude", "Latitude"), 
    crs = 4326
  )
  
  return(chargers_sf)
}