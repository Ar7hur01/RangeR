#' Simple uniform Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @return Leaflet map.
#' @export
 
range_buffer_simple <- function(lon, lat, model_name, batterylevel = 100) {

  ##from Fiat 500e Cabrio to "fiat500ecabrio"
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) # Deletes any non letter/number
    tolower(x)
  }

  # 2. Search in databank
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(ev_models_all$model)

  # Does database match with user input?
  match_idx <- which(db_models_clean == user_input_clean)

  if (length(match_idx) == 0) {
    # If nothing is found, help will show up
    stop(paste0("Model '", model_name, "' not found. Try one of these: ", 
                paste(head(ev_models_all$model, 3), collapse = ", ")))
  }

  # 3. Extract data
  selected_row <- ev_models_all[match_idx[1], ]
  # Recalculate to kilometers
  range_model <- selected_row$range_km[1] * (batterylevel/100) * 1000

  # if batterylevel is not at 100 %
    if (batterylevel > 100 || batterylevel < 0) {
    warning("Battery level should be between 0 and 100. Using default (100%).")
  }


  ### Start with map and buffer
  # 1. Create a point from given coordinates
  vehicle_pt <- sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326)

  # 2. Transform to projected CRS, then Buffer & transform back
  buffer_creation <- vehicle_pt |>
    sf::st_transform(3857) |> 
    sf::st_buffer(dist = range_model) |> 
    sf::st_transform(4326)

  # 3. OSM Map
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |>  # Adds default OSM tiles
    leaflet::addPolygons(data = buffer_creation, color = "blue", weight = 2, fillOpacity = 0.3) |> 
    leaflet::addMarkers(lng = lon, lat = lat, popup = paste0("Model: ", selected_row$model, "<br>",
                 "Battery: ", batterylevel, "%<br>",
                 "Est. Range: ", (range_model/1000), " km")
)
  return(map)
}