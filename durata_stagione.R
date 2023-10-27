# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-snow-indicators?tab=doc

# init 
setwd("~/R/turismo")
source('common.R')

# TODO ####
# - mappe della differenza sui decennio rispetto al 2014

# secondo set ####
list.files(path = ".", pattern = "^beginning.*NS.*", full.names = TRUE, recursive = TRUE) -> beg_files
list.files(path = ".", pattern = "^end-season.*NS.*", full.names = TRUE, recursive = TRUE) -> end_files
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
  # print(y)
}) -> listone

# assegno gli anni ai nomei degli slot
names(listone) <- seq(1961, 2014)

do.call(rbind, listone) %>% 
  arrange(bnuts, bzs) %>% 
  select(anno, bnuts, beos, eeos, ezs, durata) %>% 
  setNames(c("anno", "nut", "beos", "eeos", "zs", "durata")) -> df_durata
rownames(df_durata) <- NULL # ripulisco i nomi di riga

# write_ods(df_durata, "df_durata.ods")
# write_excel_csv(df_durata, "df_durata.csv")
# write.xlsx(df_durata, 'df_durata.xlsx')

# faccio il melt per plottare in base alla quota
select(df_durata, c(anno, nut, zs, durata)) %>% 
  set_names(c("Anno", "NUT", "Quota", "Durata")) %>% 
  filter(Quota >= 1500) %>% 
  reshape2::melt(id.vars = c("Anno", "NUT", "Quota")) -> m_df_durata


graf_durata <- function(nut) {

  provincia <- nome_provincia(nut)
  
  df <- filter(m_df_durata, NUT == nut)
  if(nrow(df) == 0) 
    return("")

  
  df %>% 
    ggplot(aes(Anno, value)) + 
    geom_step() + 
    geom_smooth(method = lm, se = FALSE) +
    facet_wrap(~Quota) + ylab("gg") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none") +
    ggtitle(paste(nut, provincia, sep = " - " )) -> g0
  
  
  df %>%
    group_by(Anno, NUT) %>%
    summarise(mm = mean(value)) %>%
    ggplot(aes(Anno, mm)) +
    geom_step() + ylab("gg") +
    # geom_smooth(method = "gam", formula = y ~ poly(x, 2) ) -> g1
    geom_smooth(method = "lm") -> g1
  
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,2))
  
  gridExtra::grid.arrange(g0, g1, layout_matrix = lay) 
}

# graf_durata("ITF11")

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

map(prov_int$nuts_id, function(x) {
  media_durata(x)
}) -> lst_durata


lst_durata <- vctrs::list_drop_empty(lst_durata) # rimuovo gli elementi vuoti

do.call(rbind, lst_durata) -> df_durata_media # durata media nel lustro

# map(unique(df_durata_media$NUT) %>% head(n = 2), \(x) {
#   nome_provincia(x) -> provincia
#   
#   filter(df_durata_media, NUT == x) %>% 
#     reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) %>% 
#     ggplot(aes(Lustro, value)) + 
#     geom_step() + 
#     geom_smooth(method = "gam", se = FALSE) +
#     facet_wrap(~Quota) + ylab("gg") +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       legend.position = "none") +
#     ggtitle(paste(x, provincia, sep = " - " ))
# })

# prova LM ####
# prov_int$nuts_id %>% 
#   head(1) %>% 
#   map(function(x) {
#     filter(df_durata, nut == x) -> df
#     df$quota <- factor(df$zs)
#     gam(durata ~ s(anno, by = quota), data = df)
#   }) -> test

# df_durata %>% filter(nut == "ITC11") -> tmpdf
# tmpdf$quota <- factor(tmpdf$zs)
# g <- lm(durata ~ quota + anno, data = tmpdf)
# summary(g)

summ_lm <- function(x) {
  df_durata %>% 
    filter(nut == x) -> df
  
  df$quota <- factor(df$zs)
  
  if(levels(df$quota) %>% length() > 1) {
    fit <- lm(durata ~ quota + anno, data = df)
    summ(fit)
    # cat("\n\n")
  }
}
