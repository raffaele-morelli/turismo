# init 
{
  library(ncdf4) # package for netcdf manipulation
  library(raster) # package for raster manipulation
  library(rgdal) # package for geospatial analysis
  library(ggplot2) # package for plotting
  library(dplyr)
  library(chron)
  library(lattice)
  library(RColorBrewer)
  library(sf)
  library(purrr)
  library(reshape2)
  library(terra)
  library(readODS)
  library(readr)
  library(openxlsx)
}

if(!exists("confini")) {
  confini <- st_read("shape/NUTS3_ID.shp")
}

if(!exists("prov_int")) {
  prov_int <- read_ods("province_interesse.ods")
}

nome_provincia <- function(nut) {
  filter(confini, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    select(nuts_name) %>% as.character() -> provincia
  
  return(provincia)
}




