#' Simple uniform Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric.
#' @param lat Numeric.
#' @return Leaflet map.
#' @export
 
range_buffer_simple <- function(lon, lat, radius_meters = 100000) {

  # 1. Create a point from given coordinates
  vehicle_pt <- sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326)

  # 2. Transform to projected CRS, then Buffer & transform back
  buffer_creation <- vehicle_pt |>
    sf::st_transform(3857) |> 
    sf::st_buffer(dist = radius_meters) |> 
    sf::st_transform(4326)

  # 3. OSM Map
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |>  # Adds default OSM tiles
    leaflet::addPolygons(data = buffer_creation, color = "blue", weight = 2, fillOpacity = 0.3) |> 
    leaflet::addMarkers(lng = lon, lat = lat, popup = "Vehicle Location")

  return(map)
}