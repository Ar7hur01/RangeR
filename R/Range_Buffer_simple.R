#' Simple uniform Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @return Leaflet map.
#' @export
 
range_buffer_simple <- function(lon, lat, model_name, batterylevel = 100) {

  # 1. String Cleaning (intern)
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) 
    tolower(x)
  }

  # 2. Database search
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(ev_models_all$model)
  match_idx <- which(db_models_clean == user_input_clean)

  if (length(match_idx) == 0) {
    stop(paste0("Model '", model_name, "' not found."))
  }

  # 3. Calculating range
  selected_row <- ev_models_all[match_idx[1], ]
  range_model <- selected_row$range_km[1] * (batterylevel/100) * 1000

  # 4. Geodaten (Buffer) erstellen
  vehicle_pt <- sf::st_point(c(lon, lat)) |> sf::st_sfc(crs = 4326)
  buffer_creation <- vehicle_pt |>
    sf::st_transform(3857) |> 
    sf::st_buffer(dist = range_model) |> 
    sf::st_transform(4326)

  # --- DIESE ZEILE HAT WAHRSCHEINLICH GEFEHLT ---
  chargers <- fetch_chargers_ocm(lat = lat, lon = lon, distance_km = (range_model / 1000))
  # ----------------------------------------------

  ## adding the icons
  car_icon <- leaflet::makeAwesomeIcon(
    icon = "car",           # Nutzt das Font-Awesome Auto-Symbol
    library = "fa",
    markerColor = "blue",   # Das Auto bekommt eine andere Farbe als die Ladesäulen
    iconColor = "white"
  )
  charger_icon <- leaflet::makeAwesomeIcon(
    icon = "flash",          # Alternativ: "plug" oder "bolt"
    iconColor = "white",
    markerColor = "green",   # Farbe des "Tropfens" im Hintergrund
    library = "fa"           # Font Awesome
  )
  
  # 5. Karte bauen
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |> 
    leaflet::addPolygons(data = buffer_creation, color = "blue", weight = 2, fillOpacity = 0.2)

  map <- map |>
    leaflet::addAwesomeMarkers(
      lng = lon, 
      lat = lat, 
      icon = car_icon,
      popup = paste0("<b>Fahrzeug:</b> ", selected_row$model)
    )

  # 6. Ladestationen hinzufügen (nur wenn 'chargers' existiert)
  if (!is.null(chargers)) {
    map <- map |> 
      leaflet::addAwesomeMarkers(
        data = chargers, 
        icon = charger_icon,
        group = "Charging Stations",
        clusterOptions = leaflet::markerClusterOptions() # Bonus: Gruppiert Icons beim Rauszoomen
      )
  }

  return(map)
}