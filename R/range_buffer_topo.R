#' Topographic Buffer around the vehicle location (in coordinates)
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param model_name From the Database with corresponding range-value.
#' @param batterylevel The Batterylevel (in %) of the vehicle (changes the total range), default = 100 %.
#' @param dem SpatRaster (von elevatr geladen)
#' 
calc_ev_reach <- function(start_point, battery_kwh, consumption_base, dem) {
  library(terra)
  library(gdistance) # Für die Transition-Matrix-Logik
  
  # 1. Erstelle ein Raster der benachbarten Zellen-Übergänge
  # Wir brauchen ein Raster-Objekt (terra zu raster für gdistance Kompatibilität)
  r <- as(dem, "Raster")
  
  # 2. Übergangsfunktion definieren (Kosten von Zelle a zu b)
  # x[1] ist Höhe Start, x[2] ist Höhe Ziel
  ev_cost_fun <- function(x) {
    dist <- 1 # Normalisierte Distanz
    dz <- x[2] - x[1]
    slope <- dz / dist # Sehr vereinfacht
    
    # Energieverbrauch-Logik:
    # Bergauf (dz > 0): Mehrverbrauch
    # Bergab (dz < 0): Rekuperation (Faktor 0.7 Effizienz)
    energy <- ifelse(dz > 0, 
                     consumption_base + (dz * 0.001), 
                     consumption_base + (dz * 0.0007))
    
    # Kosten müssen positiv sein für accCost, 
    # daher setzen wir ein Minimum (Auto braucht immer etwas Strom)
    return(max(energy, 0.01))
  }
  
  # 3. Transition Matrix berechnen
  # 'altDiff' übergibt die Höhendifferenz an unsere Funktion
  tr <- transition(r, transitionFunction = ev_cost_fun, directions = 8)
  tr <- geoCorrection(tr)
  
  # 4. Akkumulierte Kosten berechnen (Verbrauch von Startpunkt aus)
  start_coords <- st_coordinates(start_point)
  cost_raster <- accCost(tr, start_coords)
  
  # 5. Buffer erstellen: Wo ist der Verbrauch <= battery_kwh?
  reach_buffer <- cost_raster <= battery_kwh
  
  return(reach_buffer)
}