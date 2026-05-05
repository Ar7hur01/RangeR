#' Internal helper-function to fetch charging stations and reduce the size of the "topo_buffer"-function
#' @keywords internal
fetch_charging_stations <- function(lat, lon, radius_km, api_key) {
  
  # API call logic
  resp <- httr2::request("https://api.openchargemap.io/v3/poi/") |>
    httr2::req_url_query(
      key = api_key,
      latitude = lat,
      longitude = lon,
      distance = radius_km + 10,
      distanceunit = "KM",
      compact = "true"
    ) |>
    httr2::req_perform()

  stations_raw <- httr2::resp_body_json(resp)
  
  if (length(stations_raw) == 0) return(NULL)

  # Data cleaning
  stations_df <- data.frame(
    name  = sapply(stations_raw, function(x) x$AddressInfo$Title),
    lon   = sapply(stations_raw, function(x) x$AddressInfo$Longitude),
    lat   = sapply(stations_raw, function(x) x$AddressInfo$Latitude),
    power = sapply(stations_raw, function(x) {
      p <- x$Connections[[1]]$PowerKW
      if (is.null(p)) "Unknown" else paste0(p, " kW")
    })
  ) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  return(stations_df)
}