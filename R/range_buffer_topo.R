#' Topographic Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param z Zoom-Level for elevatr (5-8 recommended; default = 6)
#' @export

calc_ev_topo_buffer <- function(lon, lat, model_name, batterylevel = 100, z = 6) {

  # Validate Coordinates and transform into sf
  point <- data.frame(lon = lon, lat = lat) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  ## Matching input and database
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
  range_model <- selected_row$range_km[1] * (batterylevel/100)
  range_model_meter <- range_model * 1000

  # if batterylevel is not at 100 %
    if (batterylevel > 100 || batterylevel < 0) {
    warning("Battery level should be between 0 and 100. Using default (100%).")
  }

  point_metric <- sf::st_transform(point, crs = 3857)
  # 3. DEM Download via elevatr
  message("Downloading DEM...")
  dem <- elevatr::get_elev_raster(
    locations = point_metric, 
    z = z,            
    clip = "bbox", 
    expand = range_model_meter
  )  
  dem_terra <- terra::rast(dem)

  ## Generate slopemap from DEM
  slope <- terra::terrain(dem_terra, v = "slope", unit = "degrees")
  terra::plot(slope, main = "Slope in degree")

  ## Generate the buffer based on the topography and the carmodel's consumption/batterysize
  available_battery <- as.numeric(selected_row$batterysize)
  consumption <- as.numeric(selected_row$consumption)


#  flat_range <- available_battery/(consumption/1000)

  ## Height-model --> consumption rises
#  slope_factor <- 1 + (slope_degree * 0.22)
  
  ## Incline-model --> consumption decreases
#  if (slope_degree < 0) {slope_factor <- 1 + (slope_degree * 0.22 * 0.7)}
#  slope_range <- flat_range * slope_factor

  
  
  
  # 1. Slope in Prozent umrechnen (da unsere 0.22-Regel auf % basiert)
# Formel: tan(grad * pi / 180) * 100
slope_pct <- tan(slope * pi / 180) * 100

# 2. Den Slope-Faktor als neues Raster berechnen
# Wir nutzen ifel (Raster-IF), um Steigung und Gefälle zu unterscheiden
s_factor_raster <- terra::ifel(slope_pct >= 0,
                               1 + (slope_pct * 0.22),           # Bergauf
                               1 + (slope_pct * 0.22 * 0.7))     # Bergab (Rekuperation)

# 3. Den lokalen Verbrauch pro km berechnen (Wh/km)
# consumption ist hier Wh/km (falls dein Input kWh/100km ist, vorher umrechnen)
local_consumption <- consumption * s_factor_raster

  
# 4. Reichweiten-Potenzial pro Pixel (theoretisch)
# Das zeigt dir, wie effizient ein Auto in diesem spezifischen Gelände-Pixel ist
local_range_map <- available_battery / local_consumption

terra::plot(local_range_map, main = "Lokale Reichweiteneffizienz (km)")
  


#    map <- leaflet::leaflet() |> 
#    leaflet::addTiles() |>  # Adds default OSM tiles
#    leaflet::addPolygons(data = dem, color = "blue", weight = 2, fillOpacity = 0.3) |> 
#    leaflet::addMarkers(lng = lon, lat = lat, popup = paste0("Model: ", selected_row$model, "<br>",
#                 "Battery: ", batterylevel, "%<br>",
#                 "Est. Range: ", (range_model/1000), " km")
#)
#  return(map)
}