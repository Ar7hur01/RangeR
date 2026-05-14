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
  
  # Check, ob der Key vorhanden ist
  if (api_key == "") {
    warning("Kein OpenChargeMap API-Key gefunden! Bitte in der .Renviron als OCM_API_KEY hinterlegen.")
    return(NULL)
  }
  
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
  
  res <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    message("Fehler bei der API-Abfrage: ", e$message)
    return(NULL)
  })
  
  if (is.null(res) || length(res) == 0 || !"AddressInfo" %in% names(res)) {
    return(NULL)
  }
  
  # Umwandlung in sf-Objekt
  chargers_sf <- sf::st_as_sf(
    res$AddressInfo, 
    coords = c("Longitude", "Latitude"), 
    crs = 4326
  )
  
  return(chargers_sf)
}