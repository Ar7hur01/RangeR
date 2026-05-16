#' Simple uniform Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param temp_celsius If the temperature is lower than 20°C it reduces the total range by 1.5 % per degree (default = 20°C)
#' @return Leaflet map.
#' @export
 
range_buffer_simple <- function(lon, lat, model_name, batterylevel = 100, temp_celsius = 20) {
  
  ## Looking into the "Package-database and into the custom created one to search for the modelnames
  all_available_models <- ev_models_all
  user_data_path <- file.path(tools::R_user_dir("RangeR", which = "data"), "custom_models.csv")

  if (file.exists(user_data_path)) {
    custom_models <- read.csv(user_data_path, stringsAsFactors = FALSE)
    # Combine the dataframes (based on colnames)
    all_available_models <- rbind(all_available_models, custom_models)
  }

  # generic String Cleaning 
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) 
    tolower(x)
  }

  # Database search
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(all_available_models$model)
  match_idx <- which(db_models_clean == user_input_clean)

  if (length(match_idx) == 0) {
    stop(paste0("Model '", model_name, "' not found."))
  }

  # Adding the temperature influence (<20°C)
  temp_factor <- if (temp_celsius < 20) {
  1 - (20 - temp_celsius) * 0.015
  } else {
  1 # >20°C has no influence
  }

  # Calculating range (based on batterylevel)
  selected_row <- all_available_models[match_idx[1], ]
  range_model <- selected_row$range_km[1] * (batterylevel/100) * 1000 * temp_factor

  print(range_model)

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