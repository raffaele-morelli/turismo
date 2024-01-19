# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-snow-indicators?tab=doc

# init 
setwd("~/R/turismo")
source('common.R')


# attributi ####
{
  # 5 variables (excluding dimension variables):
  #   float LAT[Number_of_points]   
  # units: degrees_north
  # _FillValue: -9999999
  # long_name: latitude
  # float LON[Number_of_points]   
  # units: degrees_east
  # _FillValue: -9999999
  # long_name: longitude
  # char NUTS3_ID[nchar,Number_of_points]   
  # long_name: NUTS regions
  # out_name: nuts
  # type: character
  # float ZS[Number_of_points]   
  # units: m
  # _FillValue: -9999999
  # long_name: altitude
  # float mm_prod[Number_of_points,time]   
  # long_name: Annual amount of machine made snow produced (in kg m-2)
  # units: kg m-2
  # _FillValue: 1.00000002004088e+20
  # missing_value: 1.00000002004088e+20
  # standard_name: mm_prod
  # comment: Value of the total amount of snow produced on July 31st of year N (using managed snow simulations)
  # out_name: mm_prod
  # type: real
  # 
  # 3 dimensions:
  #   Number_of_points  Size:6584 (no dimvar)
  # nchar  Size:5 (no dimvar)
  # time  Size:1   *** is unlimited *** 
  #   standard_name: time
  # units: days since 1961-08-01 06:00:00
  # calendar: standard
  # axis: T
  # 
  # 9 global attributes:
  #   CDI: Climate Data Interface version 1.8.2 (http://mpimet.mpg.de/cdi)
  # Conventions: CF-1.7
  # CDO: Climate Data Operators version 1.8.2 (http://mpimet.mpg.de/cdo)
  # NCO: netCDF Operators version 4.7.9 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)
  # creation_date: 2020-02-18T14:27:20Z
  # frequency: year
  # contact: samuel.morin@meteo.fr
  # institute_id: CNRM
  # institute: Centre National de Recherches Météorologiques
}

# secondo set ####
list.files(path = ".", pattern = "^mm-prod.*", full.names = TRUE, recursive = TRUE) -> mm_files

# nc_open(mm_files[1])

map(mm_files, function(m) {
  stringr::str_split(m, "_")[[1]][5] %>% as.numeric() -> yname
  # print(yname)
  mm_data <- nc_open(m)
 
  mm_prod <- ncvar_get(mm_data, "mm_prod")
  nuts <- ncvar_get(mm_data, "NUTS3_ID")
  zs <- ncvar_get(mm_data, "ZS")
  
   
  data.frame("anno" = yname, nuts, zs, mm_prod) %>%
    filter(grepl("^IT", nuts))
}) -> mm_list


names(mm_list) <- seq(1961, 2014)

do.call(rbind, mm_list) %>% 
  arrange(nuts, zs) -> df_machine_made

rownames(df_machine_made) <- NULL

# write_excel_csv(df_machine_made, "df_machine_made.csv")
# write_ods(df_machine_made, "df_machine_made.ods")
# write.xlsx(df_machine_made, "df_machine_made.xlsx")
saveRDS(df_machine_made, file = "rds/df_machine_made.RDS")

dplyr::select(df_machine_made, c(anno, nuts, zs, mm_prod)) %>% 
  set_names(c("Anno", "NUT", "Quota", "Machine made")) %>% 
  filter(Quota >= 1500) %>% 
  reshape2::melt(id.vars = c("Anno", "NUT", "Quota")) -> m_df_machine_made
saveRDS(m_df_machine_made, file = "rds/m_df_machine_made.RDS")


graf_mm <- function(nut) {
  filter(confini, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    select(nuts_name) %>% as.character() -> provincia
  
  df <- filter(m_df_machine_made, NUT == nut)
  if(nrow(df) == 0) 
    return("")
  
  cat("\n\n")
  cat(paste("## ", nut, provincia, sep = " " ))
  cat("\n\n")
  
  filter(m_df_machine_made, NUT == nut) %>% 
    ggplot(aes(Anno, value, fill = Quota)) + 
    geom_step() + 
    facet_wrap(~Quota) +
    geom_smooth(method = lm, se = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") + ylab("kg/m²") +
    ggtitle(paste(nut, provincia, sep = " - " )) -> g0
  
  
  m_df_machine_made %>% filter(NUT == nut) %>% 
    group_by(Anno, NUT) %>% 
    summarise(mm = mean(value)) %>%
    ggplot(aes(Anno, mm)) + 
    geom_step() + ylab("kg/m²") + 
    geom_smooth(method = lm, se = FALSE) +
    ggtitle("Totale") -> g1
  
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,2))
  
  gridExtra::grid.arrange(g0, g1, layout_matrix = lay)
}


media_innevamento <- function(nut) {
  nome_provincia(nut) -> provincia
  
  df <- dplyr::filter(m_df_machine_made, NUT == nut)
  if(nrow(df) == 0) {
    return()
  }
  
  df %>% 
    dplyr::mutate(Lustro = Anno - Anno %% 5) %>% 
    dplyr::group_by(NUT, Lustro, Quota) %>% 
    dplyr::summarise(media = mean(value) )
}

# nut <- "ITF11"

map(prov_int$nuts_id, function(x) {
  media_innevamento(x)
}) -> lst_machine_made


lst_machine_made <- vctrs::list_drop_empty(lst_machine_made) # rimuovo gli elementi vuoti

do.call(rbind, lst_machine_made) -> df_machine_made_lustro # machine_made media nel lustro
saveRDS(df_machine_made_lustro, file = "~/R/turismo/rds/df_machine_made_lustro.RDS")

# graf_mm("ITF11")

# nut <- "ITF11"
# 
# filter(confini, nuts_id == nut) %>% 
#   st_drop_geometry() %>% 
#   select(nuts_name) %>% as.character() -> provincia
# 
# filter(m_df, NUT == nut) %>% 
#   ggplot(aes(Anno, value, fill = Quota)) + 
#   geom_step() + 
#   facet_wrap(~Quota) +
#   geom_smooth(method = lm, se = FALSE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") + ylab("kg/m²") +
#   ggtitle(paste(nut, provincia, sep = " - " )) -> g0
# 
# 
# m_df %>% filter(NUT == nut) %>% 
#   group_by(Anno, NUT) %>% 
#   summarise(mm = mean(value)) %>%
#   ggplot(aes(Anno, mm)) + 
#   geom_step() + ylab("kg/m²") + 
#   geom_smooth(method = lm, se = FALSE) +
#   ggtitle("Totale") -> g1
# 
# lay <- rbind(c(1,1),
#              c(1,1),
#              c(2,2))
# 
# gridExtra::grid.arrange(g0, g1, layout_matrix = lay)
#   