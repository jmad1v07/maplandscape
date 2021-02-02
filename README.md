
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maplandscape

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

A dashboard application to explore geospatial data using interactive
tables, web maps, and charts. map.landscape is developed using the
<a href="https://shiny.rstudio.com" target="_blank">Shiny</a> framework.

The motivation to develop map.landscape was to quickly explore and sync
data collected using
<a href="https://qfield.org" target="_blank">QField</a> mobile GIS.
However, it supports spatial and non-spatial data in GeoPackage files
and is relevant for a range of data exploration and visualisation use
cases. map.landscape forms part of workflow developed by the
<a href="https://livelihoods-and-landscapes.com" target="_blank">ACIAR
Livelihoods and Landscapes</a> project to map agricultural landscapes
and record information about farm management and condition.

  - Sync data collected using QField on different mobile devices.
  - Upload spatial and non-spatial tabular data as GeoPackage or zip
    files.
  - Explore data in interactive tables (using
    <a href="https://rstudio.github.io/DT/" target="_blank">DataTables</a>).
  - Combine tabular data using spatial and non-spatial joins.
  - Create summary tables.
  - Create and style web maps (using
    <a href="https://rstudio.github.io/leaflet/" target="_blank">Leaflet</a>).
  - Visualise data using different charts (using
    <a href="https://ggplot2.tidyverse.org" target="_blank">ggplot2</a>).
