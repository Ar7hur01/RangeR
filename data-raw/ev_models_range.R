# data-raw/ev_models.R

# 1. CSV einlesen (UTF-8 ist meist am sichersten für Modellnamen)
ev_raw <- read.csv2("data-raw/battery_range_consumption.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

## 2. Change all Website-blanks to normal blanks
#ev_raw$Model <- gsub("[[:space:]]", " ", ev_raw$Model)

# 3. Daten bereinigen (Stelle sicher, dass die Spaltennamen stimmen)
# Wir nennen sie hier 'model' und 'range_km'
ev_models_all <- ev_raw[, c("Model","Batterysize","Range..avg...388.km.","Consumption")] ##special signs as "."
colnames(ev_models_all) <- c("model", "batterysize", "range_km", "consumption")
str(ev_models_all)

# 4. In das Paket-Format (.rda) umwandeln
usethis::use_data(ev_models_all, overwrite = TRUE)
