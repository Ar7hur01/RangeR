
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

# 1. Calculate a simple range buffer considering temperature
simple_range <- range_buffer_simple(
  lon = 9.932, 
  lat = 49.791, 
  model = "Tesla Model 3 Long Range", 
  batterylevel = 80, 
  temperature = 15
)

# 2. Advanced calculation including topography (DEM) and routing
# topo_range <- calc_ev_topo_buffer(lon = 9.932, lat = 49.791, model = "Tesla Model 3")
```
