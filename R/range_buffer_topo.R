#' Topographic Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name Name des E-Autos (muss in interner DB existieren)
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param z Zoom-Level for elevatr (9-12 recommended)
#' @export
calc_ev_topo_buffer <- function(lon, lat, model_name, start_point, z = 9) {

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
  # 3. DEM Download via elevatr
  # Wir erstellen eine Bounding Box oder einen Buffer um den Punkt
  message("Lade Höhendaten (DEM) herunter...")
  
  dem <- elevatr::get_elev_raster(
    locations = point, 
    z = 7,            
    clip = "bbox", 
    expand = 5
  )  
}