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
  
  # df_durata <- readRDS("~/R/turismo/rds/df_durata.RDS")
  # m_df_durata <- readRDS("~/R/turismo/rds/m_df_durata.RDS")
  # df_durata_media <- readRDS("~/R/turismo/rds/df_durata_media.RDS")
  # df_machine_made <- readRDS("~/R/turismo/rds/df_machine_made.RDS")  
  # m_df_machine_made <- readRDS("~/R/turismo/rds/m_df_machine_made.RDS") 
  
  rds.files <- list.files(path = "~/R/turismo/rds", full.names = TRUE)
  for (f in rds.files) {
    dfname <- tools::file_path_sans_ext(f) %>% basename() 
    assign(paste0(dfname), readRDS(f))
  }

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

  filter(m_df_machine_made, NUT == nut) %>% 
    ggplot(aes(Anno, value, fill = Quota)) + 
    geom_step() + 
    facet_wrap(~Quota, scales = "free") +
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
    facet_wrap(~Quota) + ylab("Durata stagione (gg)") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none") +
    ggtitle(nome_provincia(nut)) -> g0
  
  
  df %>%
    group_by(Anno, NUT) %>%
    summarise(mm = mean(value)) %>%
    ggplot(aes(Anno, mm)) +
    geom_step() + ylab("Durata stagione (gg)") +
    # geom_smooth(method = "gam", formula = y ~ poly(x, 2) ) -> g1
    geom_smooth(method = "lm", se = FALSE) -> g1
  
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
    filter(nut == x, zs >= 1500) -> df
  
  df$quota <- factor(df$zs)
  if(nrow(df) > 0){
    if(levels(df$quota) %>% length() > 1) {
      fit <- gam(durata ~ anno + quota, data = df)
      summary(fit)
    }
  }
}
# summ_gam("ITC44")

# durata ####
# map(prov_int$nuts_id, \(p) {
#   filter(df_durata, nut == p)
# })


# durata stimata ####
map(prov_int$nuts_id, \(p) {
  summ_gam_durata(p)$p.coeff
}) -> duratone

names(duratone) <- prov_int$nuts_id

duratone <- duratone[lengths(duratone) != 0]

v_res <- seq(1300, 3500, by = 100) # risoluzione verticale

app <- data.frame("quota" = v_res)
app$quota <- paste0("quota", app$quota)

map(duratone, \(d) {
  as.data.frame(d) %>% 
    tibble::rownames_to_column(var = "quota") -> tmp

  left_join(app, tmp, by = "quota") %>% dplyr::select(d) %>% t()
}) -> tmp

do.call(rbind.data.frame, tmp) %>% dplyr::select(-c(1)) -> tab_durata_stagione

# mappe durata ####
filter(df_durata_lustro, Lustro == 1960 | Lustro == 2010) %>% 
  melt(id.vars = c("NUT","Quota", "Lustro", "media")) %>% head()

inner_join(
  filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
  filter(df_durata_lustro, Lustro == 1960 | Lustro == 2010),
  join_by(nuts_id == NUT) 
) -> pippo


brks <- classInt::classIntervals(pippo$media, style = "pretty", n = 9)
pippo <- pippo %>% mutate(pippo, Durata = cut(media, breaks = brks$brks, include.lowest = TRUE))

ggplot() + 
  geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
  geom_sf(data = pippo, aes(fill = Durata), na.rm = FALSE) + 
  scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
  # scale_fill_gradient(guide = guide_legend(title = "xxxx"), low = "#e3d662", high = "#918836", na.value = "transparent") +
  facet_wrap(vars(Lustro) ) + theme_void()


# mappe innevamento ####
filter(df_machine_made_lustro, Lustro == 1960 | Lustro == 2010) %>% 
  melt(id.vars = c("NUT","Quota", "Lustro", "media")) %>% head()

inner_join(
  filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
  filter(df_machine_made_lustro, Lustro == 1960 | Lustro == 2010),
  join_by(nuts_id == NUT) 
) -> pluto


brks <- classInt::classIntervals(pluto$media, style = "pretty", n = 9)
pluto <- pluto %>% mutate(pluto, Qtà = cut(media, breaks = brks$brks, include.lowest = TRUE))

ggplot() + 
  geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
  geom_sf(data = pluto, aes(fill = Qtà), na.rm = FALSE) + 
  scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
  # scale_fill_gradient(guide = guide_legend(title = "xxxx"), low = "#e3d662", high = "#918836", na.value = "transparent") +
  facet_wrap(vars(Lustro, Quota) ) + theme_void()


# t(tab_durata_stagione) %>% 
#   as_tibble() %>% 
#   mutate(dplyr::across(c("ITC43", "ITC42")), lag)

# lag(t(tab_durata_stagione)) %>% round()


# t(tab_durata_stagione) %>% round() %>% View()


# dplyr::select(df_durata, c(anno, nut, zs, durata)) %>% 
#   set_names(c("Anno", "NUT", "Quota", "Durata")) %>% 
#   filter(Quota >= 1200, NUT %in% prov_int$nuts_id) %>% 
#   reshape2::melt(id.vars = c("Anno", "NUT", "Quota")) %>% 
#   # filter(m_df, NUT == "ITC11") %>% 
#   ggplot() + 
#   geom_col(aes(Anno, value, fill = Quota), position = "dodge") + 
#   geom_smooth(aes(Anno, value), method = "lm") +
#   facet_wrap(~NUT)
# 
# dplyr::select(df_durata_media_lustro, c(Lustro, NUT, Quota, media)) %>% 
#   filter(Quota >= 1500, NUT %in% prov_int$nuts_id) %>% 
#   reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) %>% 
#   ggplot() + 
#   geom_col(aes(Lustro, value, fill = Quota), position = "dodge") + 
#   geom_smooth(aes(Lustro, value), method = "lm", se = FALSE) +
#   facet_wrap(~NUT)
