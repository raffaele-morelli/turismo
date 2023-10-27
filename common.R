# init 
{
  library(ncdf4) # package for netcdf manipulation
  # library(raster) # package for raster manipulation
  library(rgdal) # package for geospatial analysis
  library(ggplot2) # package for plotting
  library(dplyr)
  library(chron)
  # library(lattice)
  library(RColorBrewer)
  library(sf)
  library(purrr)
  library(reshape2)
  # library(terra)
  library(readODS)
  library(readr)
  library(openxlsx)
  # library(jtools)
  library(mgcv)
  library(pander)
  
  # library(gtsummary)
  # library(sjPlot)
  # library(sjmisc)
  # library(sjlabelled)
}

if(!exists("confini")) {
  confini <- st_read("~/R/turismo/shape/NUTS3_ID.shp")
}

if(!exists("prov_int")) {
  # prov_int <- read_ods("~/R/turismo/province_interesse.ods")
  # write_csv(prov_int, "province_interesse.csv")
}

nome_provincia <- function(nut) {
  filter(confini, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    select(nuts_name) %>% as.character() -> provincia
  
  return(provincia)
}




