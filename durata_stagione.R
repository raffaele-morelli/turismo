# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-snow-indicators?tab=doc

# init 
setwd("~/R/turismo")
source('common.R')


# secondo set ####
list.files(path = "~/R/turismo/durata_stagione", pattern = "^beginning.*NS.*", full.names = TRUE, recursive = TRUE) -> beg_files
list.files(path = "~/R/turismo/durata_stagione", pattern = "^end-season.*NS.*", full.names = TRUE, recursive = TRUE) -> end_files
# nc_open(end_files[1])

# leggo tutti i file in  una lista
map2(beg_files, end_files, function(b, e) {
  stringr::str_split(b, "_")[[1]][4] -> yname
  
  b_data <- nc_open(b)
  e_data <- nc_open(e)
  
  beos <- ncvar_get(b_data, "beginning_end_of_season")
  bnuts <- ncvar_get(b_data, "NUTS3_ID")
  bzs <- ncvar_get(b_data, "ZS")

  eeos <- ncvar_get(e_data, "beginning_end_of_season")
  enuts <- ncvar_get(e_data, "NUTS3_ID")
  ezs <- ncvar_get(e_data, "ZS")

  data.frame("anno" = as.numeric(yname), bnuts, beos, bzs, enuts, eeos, ezs, "durata" = eeos - beos) %>%
    filter(grepl("^IT", bnuts))
  # print(yname)
}) -> listone

# assegno gli anni ai nomei degli slot
names(listone) <- seq(1961, 2014)

do.call(rbind, listone) %>% 
  arrange(bnuts, bzs) %>% 
  dplyr::select(anno, bnuts, beos, eeos, ezs, durata) %>% 
  setNames(c("anno", "nut", "beos", "eeos", "zs", "durata")) -> df_durata
rownames(df_durata) <- NULL # ripulisco i nomi di riga
saveRDS(df_durata, "~/R/turismo/rds/df_durata.RDS")

write_ods(df_durata, "df_durata.ods")
# write_excel_csv(df_durata, "df_durata.csv")
# write.xlsx(df_durata, 'df_durata.xlsx')

# faccio il melt per plottare in base alla quota
dplyr::select(df_durata, c(anno, nut, zs, durata)) %>% 
  set_names(c("Anno", "NUT", "Quota", "Durata")) %>% 
  filter(Quota >= 1000) %>% 
  reshape2::melt(id.vars = c("Anno", "NUT", "Quota")) -> m_df_durata
saveRDS(m_df_durata, "~/R/turismo/rds/m_df_durata.RDS")


# media sul quinquennio ####
media_durata <- function(nut) {
  nome_provincia(nut) -> provincia
  
  df <- filter(m_df_durata, NUT == nut)
  if(nrow(df) == 0) {
    return()
  }

  df %>% 
    mutate(Lustro = Anno - Anno %% 5) %>% 
    group_by(NUT, Lustro, Quota) %>% 
    summarise(media = mean(value) )
}

map(prov_int, function(x) {
  media_durata(x)
}) -> lst_durata


lst_durata <- vctrs::list_drop_empty(lst_durata) # rimuovo gli elementi vuoti

do.call(rbind, lst_durata) -> df_durata_media_lustro # durata media nel lustro
saveRDS(df_durata_media_lustro, file = "~/R/turismo/rds/df_durata_lustro.RDS")


df_durata_media_lustro %>% 
  filter(Quota >= 1000) %>% 
  reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) -> m_df_durata_lustro
saveRDS(m_df_durata, "~/R/turismo/rds/m_df_durata_lustro.RDS")


