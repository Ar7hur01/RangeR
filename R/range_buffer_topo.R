#' Topographic Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param z Zoom-Level for elevatr (6-8 recommended; default = 7)
#' @export

calc_ev_topo_buffer <- function(lon, lat, model_name, batterylevel = 100, z = 7) {

  # Validate Coordinates and transform into sf
  point <- data.frame(lon = lon, lat = lat) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) ##CRS: 4326 (WGS84, in degree)
  
  ## Looking into the "Package-database but also into the custom created one to search for the modelnames"
  all_available_models <- ev_models_all
  user_data_path <- file.path(tools::R_user_dir("RangeR", which = "data"), "custom_models.csv")

  if (file.exists(user_data_path)) {
    custom_models <- read.csv(user_data_path, stringsAsFactors = FALSE)
    # Combine models
    # Note: Ensure column names match exactly!
    all_available_models <- rbind(all_available_models, custom_models)
  }

  ## Matching input and database
  ##from Fiat 500e Cabrio to "fiat500ecabrio"
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) # Deletes any non letter/number
    tolower(x)
  }
    # 2. Search in databank
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(all_available_models$model)
  # Does database match with user input?
  match_idx <- match(user_input_clean, db_models_clean)

  if (is.na(match_idx)) {
    # If nothing is found, help will show up
    stop(paste0("Model '", model_name, "' not found. Try one of these: ", 
                paste(head(all_available_models$model, 3), collapse = ", ")))
  }

  # if batterylevel is not at 100 %
    if (batterylevel > 100 || batterylevel < 0) {
    warning("Battery level should be between 0 and 100. Using default (100%).")
      batterylevel <- 100
    }

  selected_row <- all_available_models[match_idx[1], ] ## saving the whole selected row, not just the range-value
  range_model <- selected_row$range_km[1] * (batterylevel/100)
  range_model_meter <- range_model * 1000


   ## Battery & Consumption values of the car
  available_battery <- as.numeric(selected_row$batterysize) * (batterylevel/100) ## in kWh (e.g. 72 kWh)
  available_battery_Wh <- available_battery * 1000 ## in Wh (e.g. 72000 Wh)
  consumption <- as.numeric(selected_row$consumption) ## in Wh/km (e.g. 135 Wh/km)
  consumption_per_meter <- consumption/1000 ## in Wh/m (e.g. 135 WH/m)

  ## calculate the range but based on the batterysize and consumption
  range_battery_meter <- available_battery_Wh/consumption_per_meter ## range battery in km
  point_metric <- sf::st_transform(point, crs = 25832)

  # 3. DEM Download via elevatr
  message(paste("Available battery (kWh):", available_battery_Wh/1000))
  message(paste("Consumption (kWh/km):", consumption/10))
  message(paste("Calculated Range (km):", range_battery_meter/1000))

  # Sicherheits-Check
  if (is.na(range_battery_meter)) {
  stop("Calculated Range is NA!, Check the EV-data.")
  }
  message("Downloading DEM...")
  dem <- elevatr::get_elev_raster(
    locations = point_metric, 
    z = z,            
    clip = "bbox", 
    expand = range_battery_meter + 50000
  )  
  dem_terra <- terra::rast(dem)

  # Kalibrierung auf die räumliche Skala
  # Durch die unterschiedlichen z-Werte verändert sich ja auch die Pixelgröße und dadurch die Anzahl der "Auf- und Abs"
  res_m <- mean(terra::res(dem_terra)) # Mittlere Auflösung in Metern
  base_slope_factor <- 0.22
  
  # Skalierung: Je kleiner die Auflösung (feineres Raster), desto kleiner der Faktor.
  # Beispiel: Wir nehmen 300m (ca. z=6) als Referenzpunkt.
  adj_factor <- base_slope_factor * (res_m / 300)^0.5 # Quadratwurzel dämpft den Effekt etwas
  
  ## gdistance is outdated and depending on raster --> switching to "leastcostpath"
  ecar_cost_function <- function(x) {
    # dz/dx in Prozent umwandeln
    slope_pct <- x * 100 
  
  # Dein Steigungs-Faktor (richtungsabhängig!)
    s_factor <- ifelse(slope_pct >= 0, 
                       1 + (slope_pct * adj_factor),           # Downhill
                       1 + (slope_pct * adj_factor * 0.77))           # Uphill
  
    s_factor <- pmax(s_factor, 0.3) ## Protection to avoid perpetuum mobile -> 30% energy is always used, also when going downhill
    
    consumption <- as.numeric(selected_row$consumption) ## in Wh/km (e.g. 135 Wh/km)
    consumption_per_meter <- consumption/1000 ## in Wh/m (e.g. 135 WH/m)
    cost_wh_per_meter <- consumption_per_meter * s_factor
    return(1 / cost_wh_per_meter)
  
  }

  print("Zellgröße des DEM:")
  print(terra::res(dem_terra))

  ## Cost per meter
    #cost_hills_per_meter <- consumption_per_meter * s_factor  ## in Wh/m (e.g. hilly: 135 * 10 *0.22 = 297 WH/m)
  ## Batterysize stays the same
    #range_battery_hills_meter <- available_battery_Wh/cost_hills_per_meter ## range battery (incl. percentage) in km (after hills-check)
    #return(1/cost_hills_per_meter)

  # Leitfähigkeit (1 / Kosten) berechnen
  # Kosten = Verbrauch pro Meter * Steigungsfaktor
    #conductance <- 1 / (consumption_per_meter * s_factor)
  
    #return(conductance)
  
  # Test: Was verbraucht das Auto bei 5% Steigung pro Meter?
  print(paste("Kosten bei 5% Steigung:", 1 / ecar_cost_function(0.1), "Wh/m"))


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
  reachable_raster <- terra::ifel(cost_surface <= available_battery_Wh, 1, NA)
  
  # 5. Raster in ein glattes sf-Polygon umwandeln
  reachable_poly <- terra::as.polygons(reachable_raster, dissolve = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_set_crs(25832) |>  # WICHTIG: Sagt R, dass das Polygon noch metrisch ist
    sf::st_transform(4326)   # Transformiert es für Leaflet in Längen-/Breitengrade
  

   normal_buffer <- point_metric |>
    sf::st_transform(25832) |> 
    sf::st_buffer(dist = range_battery_meter) |> 
    sf::st_transform(4326)

  # 6. Die interaktive Karte bauen
  map <- leaflet::leaflet() |> 
    leaflet::addTiles() |>
    leaflet::addPolygons(data = normal_buffer, color = "blue", weight = 2, fillOpacity = 0.1) |> 
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