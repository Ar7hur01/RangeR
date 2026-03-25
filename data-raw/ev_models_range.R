# data-raw/ev_models.R

# 1. CSV einlesen (UTF-8 ist meist am sichersten für Modellnamen)
ev_raw <- read.csv2("data-raw/Database_models_range.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

## 2. Change all Website-blanks to normal blanks
#ev_raw$Model <- gsub("[[:space:]]", " ", ev_raw$Model)

# 3. Daten bereinigen (Stelle sicher, dass die Spaltennamen stimmen)
# Wir nennen sie hier 'model' und 'range_km'
ev_models_all <- ev_raw[, c("Model","Range..avg...388.km.")] ##special signs as "."
colnames(ev_models_all) <- c("model", "range_km")
str(ev_models_all)

# 4. In das Paket-Format (.rda) umwandeln
usethis::use_data(ev_models_all, overwrite = TRUE)
