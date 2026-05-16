#' Add your own vehicle to the database
#' 
#' @param model Modelname of the vehicle.
#' @param batterysize Batterysize in kWh (for EVs normally between 20 and 140)
#' @param consumption Consumption in kWh/100km (for EVs normally between 14 and 25)
#' @param range_km Official WLTP Range in km (for modern EVs normally between 250 and 900)
#' @export
add_custom_ev <- function(model, batterysize, consumption, range_km) {
  
  # Path for user-data
  user_data_dir <- tools::R_user_dir("RangeR", which = "data")
  if (!dir.exists(user_data_dir)) dir.create(user_data_dir, recursive = TRUE)
  
  # adding the new file to the path
  file_path <- file.path(user_data_dir, "custom_models.csv")
  print(file_path)

  # new EV as df
  new_ev <- data.frame(
    model = model,
    batterysize = as.numeric(batterysize),
    range_km = as.numeric(range_km),
    consumption = as.numeric(consumption),
    stringsAsFactors = FALSE
  )
  
  # save it to the old file or generate new file (if it´s the first model)
  if (file.exists(file_path)) {
    old_evs <- read.csv(file_path)
    combined <- rbind(old_evs, new_ev)
    write.csv(combined, file_path, row.names = FALSE)
  } else {
    write.csv(new_ev, file_path, row.names = FALSE)
  }
  
  message(paste("Erfolgreich hinzugefügt:", model))
}