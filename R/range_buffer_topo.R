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
  match_idx <- match(user_input_clean, db_models_clean)

  if (length(match_idx) == 0) {
    # If nothing is found, help will show up
    stop(paste0("Model '", model_name, "' not found. Try one of these: ", 
                paste(head(ev_models_all$model, 3), collapse = ", ")))
  }

  # if batterylevel is not at 100 %
    if (batterylevel > 100 || batterylevel < 0) {
    warning("Battery level should be between 0 and 100. Using default (100%).")
      batterylevel <- 100
    }

  selected_row <- ev_models_all[match_idx[1], ]
  range_model <- selected_row$range_km[1] * (batterylevel/100)
  range_model_meter <- range_model * 1000


  point_metric <- sf::st_transform(point, crs = 3857)
  # 3. DEM Download via elevatr
  message("Downloading DEM...")
  dem <- elevatr::get_elev_raster(
    locations = point_metric, 
    z = z,            
    clip = "bbox", 
    expand = range_model_meter + 50000
  )  
  dem_terra <- terra::rast(dem)


  ## Generate the buffer based on the topography and the carmodel's consumption/batterysize
  kWh_battery <- as.numeric(selected_row$batterysize)
  available_battery <- kWh_battery * 1000
  consumption <- as.numeric(selected_row$consumption)
  consumption_per_meter <- consumption/1000

  
  ## gdistance is outdated and depending on raster --> switching to "leastcostpath"
  ecar_cost_function <- function(x) {
    # dz/dx in Prozent umwandeln
    slope_pct <- x * 100 
  
  # Dein Steigungs-Faktor (richtungsabhängig!)
    s_factor <- ifelse(slope_pct >= 0, 
                       1 + (slope_pct * 0.22),           # Bergauf
                       1 + (slope_pct * 0.22 * 0.7))     # Bergab
  
    s_factor <- pmax(s_factor, 0.01)
  # Leitfähigkeit (1 / Kosten) berechnen
  # Kosten = Verbrauch pro Meter * Steigungsfaktor
    conductance <- 1 / (consumption_per_meter * s_factor)
  
    return(conductance)
  }

  # 2. Kosten-Matrix direkt mit deiner Funktion erstellen
  cs_cost <- leastcostpath::create_slope_cs(
    x = dem_terra, 
    cost_function = ecar_cost_function, 
    neighbours = 8
  )
  
  # 3. Routing vom Startpunkt (Akkumulierte Kosten = Verbrauchte Energie)
  cost_surface <- leastcostpath::create_accum_cost(
    x = cs_cost, 
    origins = point_metric
  )
  
  # 4. Harter Cut: Alles, was die Batteriekapazität überschreitet, wird NA
  # Wichtig: cost_surface und available_battery müssen hier die SELBE Einheit haben (z.B. Wh)!
  reachable_raster <- terra::ifel(cost_surface <= available_battery, 1, NA)
  
  # 5. Raster in ein glattes sf-Polygon umwandeln
  reachable_poly <- terra::as.polygons(reachable_raster, dissolve = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_set_crs(3857) |>  # WICHTIG: Sagt R, dass das Polygon noch metrisch ist
    sf::st_transform(4326)   # Transformiert es für Leaflet in Längen-/Breitengrade
  
  # 6. Die interaktive Karte bauen
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |> 
    leaflet::addPolygons(
      data = reachable_poly, 
      color = "green", 
      weight = 2, 
      fillOpacity = 0.3
    ) |> 
    leaflet::addMarkers(
      lng = lon, lat = lat, 
      popup = paste0("Model: ", selected_row$model, "<br>",
                     "Battery: ", batterylevel, "%<br>",
                     "Flat Range: ", round(range_model, 1), " km")
    )
  
  return(map)
} # Ende der Funktion
  ## PROBLEM: Je höher die Auflösung ist, desto mehr Pixel sind im Plot und desto stärker sinkt die Range, weil
  # die Formel nach Energieverbrauch zwischen den Pixeln geht.











  #cs <- leastcostpath::create_distance_cs(dem_terra, neighbours = 8)
  #slope <- terra::terrain(dem_terra, v = "slope", unit = "degrees")
  #slope_pct <- tan(slope * pi / 180) * 100

  #s_factor <- ifelse(slope_pct >= 0, 
  #                 1 + (slope_pct * 0.22), 
  #                 1 + (slope_pct * 0.22 * 0.7))
  
  #local_cost <- consumption_per_meter * s_factor
  #cs_cost <- cs
  #cs_cost$conductanceMatrix <- 1 / local_cost

  # 4. Akkumulierte Kostenkarte berechnen (Routing vom Startpunkt)
  #cost_surface <- leastcostpath::create_accum_cost(x = cs_cost, origins = point_metric)


  
if (FALSE) {
  
tr <- gdistance::transition(dem,
  transitionFunction = function(x) {x[2] - x[1] },
  directions = 8,
  symm = FALSE)

tr_aniso <- gdistance::geoCorrection(tr, type = "c")
  
slope_vals <- tr_aniso@transitionMatrix@x
s_factor <- ifelse(slope_vals >= 0, 
                   1 + (slope_vals * 0.22), 
                   1 + (slope_vals * 0.22 * 0.7))

consumption_per_meter <- consumption/1000
  
local_cost <- consumption_per_meter * s_factor
tr_cost <- tr_aniso
tr_cost@transitionMatrix@x <- 1 / local_cost
coords_matrix <- sf::st_coordinates(point_metric)

energy_cost_map <- gdistance::accCost(tr_cost, coords_matrix)
  

  
cost_terra <- terra::rast(energy_cost_map)
reachable_raster <- terra::ifel(cost_terra <= available_battery, 1, NA)

reachable_poly <- terra::as.polygons(reachable_raster, dissolve = TRUE) |> 
  sf::st_as_sf() |> 
  sf::st_set_crs(3857) |>
  sf::st_transform(4326) 

  
map <- leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  # Das fertige Polygon hinzufügen
  leaflet::addPolygons(
    data = reachable_poly, 
    color = "green", 
    weight = 2, 
    fillOpacity = 0.3
  ) |> 
  # Deinen Startpunkt setzen
  leaflet::addMarkers(
    lng = lon, lat = lat, 
    popup = paste0("Model: ", selected_row$model, "<br>",
                   "Battery: ", batterylevel, "%")
  )

return(map)
}  
#kosten_karte <- gdistance::accCost(tr_aniso, coords_matrix)
#terra::plot(kosten_karte, main = "Lokale Reichweiteneffizienz (km)")

#r <- terra::rast(kosten_karte) 

#leaflet::leaflet() %>% 
#  leaflet::addTiles() %>%  # Standard OSM
#  leaflet::addRasterImage(r, colors = "YlOrRd", opacity = 0.6) %>%
#  leaflet::addMarkers(lng = origin_coords[1], lat = origin_coords[2], popup = "Start")

#    map <- leaflet::leaflet() |> 
#    leaflet::addTiles() |>  # Adds default OSM tiles
#    leaflet::addPolygons(data = dem, color = "blue", weight = 2, fillOpacity = 0.3) |> 
#    leaflet::addMarkers(lng = lon, lat = lat, popup = paste0("Model: ", selected_row$model, "<br>",
#                 "Battery: ", batterylevel, "%<br>",
#                 "Est. Range: ", (range_model/1000), " km")
#)
#  return(map)








  ## SLOPE --> Problem: Slope doesn´t include the direction of the slope (not useable for uphill/downhill calculation)
  ## Generate slopemap from DEM
#  slope <- terra::terrain(dem_terra, v = "slope", unit = "degrees")
  #terra::plot(slope, main = "Slope in degree")  
  
# 1. Slope in Percentage
# Formel: tan(grad * pi / 180) * 100
#slope_pct <- tan(slope * pi / 180) * 100

# 2. Den Slope-Faktor als neues Raster berechnen
# Wir nutzen ifel (Raster-IF), um Steigung und Gefälle zu unterscheiden
#s_factor_raster <- terra::ifel(slope_pct >= 0,
#                               1 + (slope_pct * 0.22),           # Bergauf
#                               1 + (slope_pct * 0.22 * 0.7))     # Bergab (Rekuperation)

# 3. Den lokalen Verbrauch pro km berechnen (Wh/km)
# consumption ist hier Wh/km (falls dein Input kWh/100km ist, vorher umrechnen)
#local_consumption <- consumption * s_factor_raster

  
# 4. Reichweiten-Potenzial pro Pixel (theoretisch)
# Das zeigt dir, wie effizient ein Auto in diesem spezifischen Gelände-Pixel ist
#local_range_map <- available_battery / local_consumption

#terra::plot(local_range_map, main = "Lokale Reichweiteneffizienz (km)")