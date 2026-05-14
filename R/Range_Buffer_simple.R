#' Simple uniform Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @return Leaflet map.
#' @export
 
range_buffer_simple <- function(lon, lat, model_name, batterylevel = 100) {

  # generic String Cleaning 
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) 
    tolower(x)
  }

  # Database search
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(ev_models_all$model)
  match_idx <- which(db_models_clean == user_input_clean)

  if (length(match_idx) == 0) {
    stop(paste0("Model '", model_name, "' not found."))
  }

  # Calculating range (based on batterylevel)
  selected_row <- ev_models_all[match_idx[1], ]
  range_model <- selected_row$range_km[1] * (batterylevel/100) * 1000

  # Buffer creation (size of the range)
  vehicle_pt <- sf::st_point(c(lon, lat)) |> sf::st_sfc(crs = 4326)
  buffer_creation <- vehicle_pt |>
    sf::st_transform(3857) |> 
    sf::st_buffer(dist = range_model) |> 
    sf::st_transform(4326)

  # calling the function fetch_chargers to get API Key for the integration
  chargers <- fetch_chargers_ocm(lat = lat, lon = lon, distance_km = (range_model / 1000))

  ## adding the icons
  car_icon <- leaflet::makeAwesomeIcon(
    icon = "car", 
    library = "fa",
    markerColor = "blue",
    iconColor = "white"
  )
  charger_icon <- leaflet::makeAwesomeIcon(
    icon = "flash",   
    iconColor = "white",
    markerColor = "green",
    library = "fa"
  )
  
  # General map creation
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |> 
    leaflet::addPolygons(data = buffer_creation, color = "blue", weight = 2, fillOpacity = 0.2)

  # adding the symbols (stations + car)
  map <- map |>
    leaflet::addAwesomeMarkers(
      lng = lon, 
      lat = lat, 
      icon = car_icon,
      popup = paste0("<b>Fahrzeug:</b> ", selected_row$model)
    )

  if (!is.null(chargers)) {
    map <- map |> 
      leaflet::addAwesomeMarkers(
        data = chargers, 
        icon = charger_icon,
        group = "Charging Stations",
        clusterOptions = leaflet::markerClusterOptions()
      )
  }

  return(map)
}