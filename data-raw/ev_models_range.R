# data-raw/ev_models.R

# Read in csv-file with all models
ev_raw <- read.csv2("data-raw/battery_range_consumption.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

## 2. Change all Website-blanks to normal blanks
#ev_raw$Model <- gsub("[[:space:]]", " ", ev_raw$Model)

# Rename columns
ev_models_all <- ev_raw[, c("Model","Batterysize","Range..avg...388.km.","Consumption")]
colnames(ev_models_all) <- c("model", "batterysize", "range_km", "consumption")
str(ev_models_all)

# change for integration to the package-format ".rda"
usethis::use_data(ev_models_all, overwrite = TRUE)