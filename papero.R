# init ####
{
  library(ggplot2) # package for plotting
  library(dplyr)
  library(chron)
  library(RColorBrewer)
  library(sf)
  library(mgcv)
  library(purrr)
  library(reshape2)
  library(readODS)
  library(readr)
  library(pander)
  
  df_durata <- readRDS("~/R/turismo/rds/df_durata.RDS")
  m_df_durata <- readRDS("~/R/turismo/rds/m_df_durata.RDS")
  df_durata_media <- readRDS("~/R/turismo/rds/df_durata_media.RDS")
  df_machine_made <- readRDS("~/R/turismo/rds/df_machine_made.RDS")  
}

if(!exists("nuts3")) {
  nuts3 <- st_read("~/R/turismo/shape/NUTS3_ID.shp")
}

if(!exists("prov_int")) {
  prov_int <- read_ods("~/R/turismo/province_interesse.ods")
  # write_csv(prov_int, "province_interesse.csv")
}

nome_provincia <- function(nut) {
  filter(nuts3, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    dplyr::select(nuts_name) %>% as.character() -> provincia
  
  return(provincia)
}

# https://ambientenonsolo.com/le-aree-di-alta-montagna-si-scaldano-piu-rapidamente-del-resto-del-globo/

graf_mm <- function(nut) {
  filter(nuts3, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    dplyr::select(nuts_name) %>% as.character() -> provincia
  
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + ylab("kg/m²") +
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


summ_lm_durata <- function(x) {
  df_durata %>% 
    filter(nut == x, zs > 1800) -> df
  
  df$quota <- factor(df$zs)
  
  if(levels(df$quota) %>% length() > 1) {
    fit <- lm(durata ~ quota + anno, data = df)
    summ(fit)
  }
}
# summ_lm("ITC44")

summ_gam_durata <- function(x) {
  df_durata %>% 
    filter(nut == x, zs >= 1700) -> df
  
  df$quota <- factor(df$zs)
  if(nrow(df) > 0){
    if(levels(df$quota) %>% length() > 1) {
      fit <- gam(durata ~ anno + quota, data = df)
      summary(fit)
    }
  }
}
# summ_gam("ITC44")

map(prov_int$nuts_id, \(p) {
  # cat(p)
  summ_gam_durata(p)$p.coeff
}) -> duratone

names(duratone) <- prov_int$nuts_id

duratone <- duratone[lengths(duratone) != 0]

app <- data.frame("quota" = seq(1700, 3300, by = 100))
app$quota <- paste0("quota", app$quota)

map(duratone, \(d) {
  as.data.frame(d) %>% 
    tibble::rownames_to_column(var = "quota") -> tmp

  left_join(app, tmp, by = "quota") %>% dplyr::select(d) %>% t()
}) -> pippo

do.call(rbind.data.frame, pippo) %>% dplyr::select(-c(1)) -> tab_durata_stagione


# df_durata %>% filter(nut == "ITC44", zs > 1800) -> df
# 
# ggplot(df, aes(anno, durata)) + geom_point() +
#   geom_smooth(method = lm, se = TRUE) +
#   facet_wrap(~zs, scales = "free")

# hist(df$durata)

# shapiro.test(df$durata)  
