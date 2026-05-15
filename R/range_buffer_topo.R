#' Topographic Buffer around the vehicle location (in coordinates)
#' 
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param z Zoom-Level for DEM-resolution (6-8 recommended; default = 7)
#' @param min_power Minimum charging speed for the charging stations (default = 50 kW)
#' @export

calc_ev_topo_buffer <- function(lon, lat, model_name, batterylevel = 100, z = 7, min_power = 50) {

  # Validate Coordinates and transform into sf
  point <- data.frame(lon = lon, lat = lat) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) ##CRS: 4326 (WGS84, in degree)
  
  ## Looking into the "Package-database and into the custom created one to search for the modelnames
  all_available_models <- ev_models_all
  user_data_path <- file.path(tools::R_user_dir("RangeR", which = "data"), "custom_models.csv")

  if (file.exists(user_data_path)) {
    custom_models <- read.csv(user_data_path, stringsAsFactors = FALSE)
    # Combine the dataframes (based on colnames)
    all_available_models <- rbind(all_available_models, custom_models)
  }

  ## Matching input and database
  ##from Fiat 500e Cabrio to "fiat500ecabrio"
  clean_string <- function(x) {
    x <- gsub("[^[:alnum:]]", "", x) # Deletes any non letter/number
    tolower(x)
  }
  # Compare user input & database --> Search in database
  user_input_clean <- clean_string(model_name)
  db_models_clean  <- clean_string(all_available_models$model)
  match_idx <- match(user_input_clean, db_models_clean)

  if (is.na(match_idx)) {
    # If nothing is found, help will show up
    stop(paste0("Model '", model_name, "' not found.\n", "Try one of these: ", 
                paste(head(all_available_models$model, 3), collapse = ", "), "\n... or run 'View(ev_models_all)' to see the full list of models."))
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

  # DEM Download via elevatr
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
    expand = range_battery_meter + 50000 # to be save in case it goes downhill alot
  )  
  dem_terra <- terra::rast(dem)

  # Calibration on a spatial scale
  # a different z-value is changing the pixelsize --> also changing the number of ups/downs
  res_m <- mean(terra::res(dem_terra)) # Mittlere Auflösung in Metern
  base_slope_factor <- 0.22
  
  # Scale: The smaller "z" is, the smaller will the factor be.
  # Taking 300m (Pixelsize of z=6) as reference.
  adj_factor <- base_slope_factor * (res_m / 300)^0.5
  
  ## gdistance is outdated and depending on raster --> switching to "leastcostpath"
  ecar_cost_function <- function(x) {
    # slope in percentage
    slope_pct <- x * 100 
  
  # Slope factor dependend on direction 
    s_factor <- ifelse(slope_pct >= 0, 
                       1 + (slope_pct * adj_factor),           # Downhill
                       1 + (slope_pct * adj_factor * 0.77))           # Uphill
  
    s_factor <- pmax(s_factor, 0.3) ## Protection to avoid perpetuum mobile -> 30% energy is always used, also when going downhill
    
    consumption <- as.numeric(selected_row$consumption) ## in Wh/km (e.g. 135 Wh/km)
    consumption_per_meter <- consumption/1000 ## in Wh/m (e.g. 135 Wh/m)
    cost_wh_per_meter <- consumption_per_meter * s_factor
    return(1 / cost_wh_per_meter)
  
  }

  # Berechnung auf einen Meter normiert
  real_wh_per_m <- (1 / ecar_cost_function(0.05))
  print(paste("Consumption at 5% Slope:", round(real_wh_per_m, 4), "Wh/m"))

  # cost/consumption of the DEM
  cs_cost <- leastcostpath::create_slope_cs(
    x = dem_terra, 
    cost_function = ecar_cost_function, 
    neighbours = 8
  )
  
  # Setting the startpoint
  cost_surface <- leastcostpath::create_accum_cost(
    x = cs_cost, 
    origins = point_metric
  )
  
  # If the battery capacity is reaches all values will turn into NA
  reachable_raster <- terra::ifel(cost_surface <= available_battery_Wh, 1, NA)
  
  # changing into sf-polygon
  reachable_poly <- terra::as.polygons(reachable_raster, dissolve = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_set_crs(25832) |>  # Metric System
    sf::st_transform(4326)   # Transformation for leaflet
  

   normal_buffer <- point_metric |>
    sf::st_transform(25832) |> 
    sf::st_buffer(dist = range_battery_meter) |> 
    sf::st_transform(4326)

  # calling the function fetch_chargers to get API Key for the integration
  chargers <- fetch_chargers_ocm(lat = lat, lon = lon, distance_km = (range_battery_meter / 1000), min_power = min_power)

  # adding the icons for the map
  car_icon <- leaflet::makeAwesomeIcon(icon = "car", library = "fa", markerColor = "blue", iconColor = "white")
  charger_icon <- leaflet::makeAwesomeIcon(icon = "flash", iconColor = "white", markerColor = "green", library = "fa")

  # Map
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
  map <- map |>
    leaflet::addAwesomeMarkers(lng = lon, lat = lat, icon = car_icon, popup = paste0("<b>Model:</b> ", selected_row$model))
  
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
