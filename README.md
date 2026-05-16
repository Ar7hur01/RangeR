
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RangeR ⚡🚙

The goal of RangeR is to calculate and visualize the realistic range of
electric vehicles (EVs) by accounting for topography, ambient
temperature, and battery levels.

It features an integrated database of over 650 modern EV models,
supports custom vehicle definitions, and utilizes Digital Elevation
Models (DEMs) to generate non-uniform, topography-aware range buffers on
an interactive map.

### Installation

You can install the development version of RangeR from GitHub with:

``` r
# install.packages("pak")
pak::pak("Ar7hur01/RangeR")
```

### Example

Here is a basic example of how to calculate a simple range buffer based
on a starting point and a specific EV model:

``` r
library(RangeR)

# To view all available models (from the database) run:
View(ev_models_all)

# 1. Calculate a simple range buffer considering temperature & batterylevel
range_buffer_simple(
  lon = 9.9782, 
  lat = 49.7873, 
  model = "BMW i5 xDrive40 Touring", 
  batterylevel = 90, 
  temp_celsius = 18
)

# 2. Advanced buffer calculation including the topography (DEM)
calc_ev_topo_buffer(
  lon = 9.43,
  lat = 48.23,
  model = "Mercedes-Benz EQS 350",
  batterylevel = 80,
  temp_celsius = 19,
  z = 6,
  min_power = 50
)


# 3. Add your own vehicle to the database
add_custom_ev <- function(model, batterysize, consumption, range_km) {
add_custom_ev(
  "My dreamcar",
  batterysize = 140,
  consumption = 14,
  range_km = 870
)
```
