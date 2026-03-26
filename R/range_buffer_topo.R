#' Topographic Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name Name des E-Autos (muss in interner DB existieren)
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param z Zoom-Level for elevatr (6-9 recommended)
#' @export

calc_ev_topo_buffer <- function(lon, lat, model_name, batterylevel = 100, z = 7) {

  # Validate Coordinates and transform into sf
  point <- data.frame(lon = lon, lat = lat) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
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

  selected_row <- ev_models_all[match_idx[1], ]
  range_model <- selected_row$range_km[1] * (batterylevel/100) * 1000

  # if batterylevel is not at 100 %
    if (batterylevel > 100 || batterylevel < 0) {
    warning("Battery level should be between 0 and 100. Using default (100%).")
  }



  # 3. DEM Download via elevatr
  message("Lade Höhendaten (DEM) herunter...")
  
  dem <- elevatr::get_elev_raster(
    locations = point, 
    z = z,            
    clip = "bbox", 
    expand = 50
  )  

  ## Generate the buffer based on the topography and the carmodel's consumption/batterysize



    map <- leaflet::leaflet() |> 
    leaflet::addTiles() |>  # Adds default OSM tiles
    leaflet::addPolygons(data = dem, color = "blue", weight = 2, fillOpacity = 0.3) |> 
    leaflet::addMarkers(lng = lon, lat = lat, popup = paste0("Model: ", selected_row$model, "<br>",
                 "Battery: ", batterylevel, "%<br>",
                 "Est. Range: ", (range_model/1000), " km")
)
  return(map)
}