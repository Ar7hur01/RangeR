#' Add your own vehicle to the database
#' 
#' @param model Modelname of the vehicle.
#' @param batterysize Batterysize in kWh (for EVs normally between 20 and 140)
#' @param consumption Consumption in kWh/100km (for EVs normally between 14 and 25)
#' @param range_km Official WLTP Range in km (for modern EVs normally between 250 and 900)
#' @export
add_custom_ev <- function(model, batterysize, consumption, range_km) {
  
  # 1. Pfad für User-Daten festlegen
  user_data_dir <- tools::R_user_dir("RangeR", which = "data")
  if (!dir.exists(user_data_dir)) dir.create(user_data_dir, recursive = TRUE)
  
  file_path <- file.path(user_data_dir, "custom_models.csv")
  print(file_path)

  # 2. Neues Fahrzeug als Dataframe
  new_ev <- data.frame(
    model = model,
    batterysize = as.numeric(batterysize),
    consumption = as.numeric(consumption*10),
    range_km = as.numeric(range_km),
    stringsAsFactors = FALSE
  )
  
  # 3. Speichern (Anhängen oder neu erstellen)
  if (file.exists(file_path)) {
    old_evs <- read.csv(file_path)
    combined <- rbind(old_evs, new_ev)
    write.csv(combined, file_path, row.names = FALSE)
  } else {
    write.csv(new_ev, file_path, row.names = FALSE)
  }
  
  message(paste("Erfolgreich hinzugefügt:", model))
}