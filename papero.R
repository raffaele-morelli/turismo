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
  library(kableExtra)
  
  
  df_durata <- readRDS("~/R/turismo/rds/df_durata.RDS")
  # m_df_durata <- readRDS("~/R/turismo/rds/m_df_durata.RDS")
  # df_durata_media <- readRDS("~/R/turismo/rds/df_durata_media.RDS")
  df_machine_made <- readRDS("~/R/turismo/rds/df_machine_made.RDS")
  # m_df_machine_made <- readRDS("~/R/turismo/rds/m_df_machine_made.RDS") 
  
  rds.files <- list.files(path = "~/R/turismo/rds", full.names = TRUE)
  for (f in rds.files) {
    dfname <- tools::file_path_sans_ext(f) %>% basename() 
    assign(paste0(dfname), readRDS(f))
  }

}

theme_turismo <- function (base_size = 11, base_family = "Arial", base_line_size = 0.25, ...)  {
  half_line <- base_size/2
  small_rel <- 0.8
  small_size <- small_rel * base_size
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace% theme(
    # rect = element_rect(fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0), 
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = ggplot2::margin(),  debug = F), 
    axis.text = element_text(size = small_size), 
    axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4), vjust = 1), 
    axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), hjust = 1), 
    axis.title.x = element_text(margin = ggplot2::margin(t = small_size, b = small_size)), 
    axis.title.y = element_text(angle = 90, margin = ggplot2::margin(r = small_size, l = small_size/4)),
    axis.ticks = element_line(colour = "black", linewidth = base_line_size), 
    axis.ticks.length = unit(0.25, "lines"), 
    axis.line = element_line(colour = "black", linewidth = base_line_size), 
    axis.line.x = element_line(colour = "black", linewidth = base_line_size), 
    axis.line.y = element_line(colour = "black", linewidth = base_line_size), 
    legend.spacing = unit(base_size/4, "pt"), 
    legend.key = element_blank(),
    legend.key.size = unit(1 * base_size, "pt"), 
    legend.key.width = unit(1.3 * base_size, "pt"), 
    legend.text = element_text(size = rel(small_rel)), 
    legend.title = element_text(size = rel(small_rel), face = "bold"), 
    # legend.position = "bottom", 
    legend.box = "horizontal", 
    panel.spacing = unit(1, "lines"), 
    # panel.background = element_blank(),
    # panel.border = element_blank(), 
    # panel.grid.major = element_blank(), 
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = base_size), 
    strip.background = element_rect(fill = NA, colour = "black", linewidth = 0.125), 
    strip.text.x = element_text(face = "bold", hjust = 0.02, margin = ggplot2::margin(b = small_size/2, t = small_size/4)), 
    strip.text.y = element_text(angle = -90, face = "bold", margin = ggplot2::margin(l = small_size/2, r = small_size/4)), 
    plot.margin = unit(c(5, 5, 0, 0), "pt"), 
    # plot.background = element_blank(),
    plot.title = element_text(face = "bold", size = 1 * base_size, margin = ggplot2::margin(b = half_line), hjust = 0))
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
    ylab("kg/m²") +
    ggtitle(paste(nut, provincia, sep = " - " ))  + theme_turismo() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") -> g0
  
  
  m_df_machine_made %>% filter(NUT == nut) %>% 
    group_by(Anno, NUT) %>% 
    summarise(mm = mean(value)) %>%
    ggplot(aes(Anno, mm)) + 
    geom_step() + ylab("kg/m²") + 
    geom_smooth(method = lm, se = FALSE) +
    ggtitle("Totale") + theme_turismo() -> g1
  
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,2))
  
  gridExtra::grid.arrange(g0, g1, layout_matrix = lay)
}
# graf_mm("ITF11")

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

# media sul quinquennio per provincia ####
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

do.call(rbind, lst_durata) -> df_durata_media_lustro # durata media nel lustro

df_durata_media_lustro %>% 
  filter(Quota >= 1500) %>% 
  reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) -> m_df_durata_lustro

map_durata <- function() {
  inner_join(
    filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
    filter(df_durata_lustro, Lustro %in% c(1960, 2010), Quota %in% seq(1600, 2800, by = 400)),
    join_by(nuts_id == NUT) 
  ) -> lustro_dur_map
  
  
  brks <- classInt::classIntervals(lustro_dur_map$media, style = "fisher", n = 9)
  lustro_dur_map <- lustro_dur_map %>% mutate(lustro_dur_map, Durata = cut(media, breaks = brks$brks, include.lowest = TRUE))
  
  ggplot() + 
    geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
    geom_sf(data = lustro_dur_map, aes(fill = Durata), na.rm = FALSE) + 
    scale_fill_brewer(palette = "Greens", na.value = "white") +
    facet_grid(vars(Lustro), vars(Quota)) + ggthemes::theme_map() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right", 
          legend.title = element_blank()) +
    ggtitle("Durata della stagione") 
}
map_durata()

map_machinemade <- function() {
  inner_join(
    filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
    filter(df_machine_made_lustro, Lustro %in% c(1960, 2010), Quota %in% seq(1600, 2800, by = 400)),
    join_by(nuts_id == NUT) 
  ) -> lustro_mm_map
  
  brks <- classInt::classIntervals(lustro_mm_map$media, style = "fisher", n = 9)
  lustro_mm_map <- lustro_mm_map %>% mutate(lustro_mm_map, Artificiale = cut(media, breaks = brks$brks, include.lowest = TRUE))
  
  ggplot() + 
    geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
    geom_sf(data = lustro_mm_map, aes(fill = Artificiale), na.rm = FALSE) + 
    scale_fill_brewer(palette = "Blues", na.value = "white") +
    facet_wrap(vars(Lustro) ) + 
    ggthemes::theme_map() + 
    ggtitle("Artificiale Kg/m²") + 
    facet_grid(vars(Lustro), vars(Quota)) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right", 
          legend.title = element_blank())
}
map_machinemade()

# mappe innevamento ####
# filter(df_machine_made_lustro, Lustro == 1960 | Lustro == 2010) %>% 
#   melt(id.vars = c("NUT","Quota", "Lustro", "media")) %>% head()

# inner_join(
#   filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
#   filter(df_machine_made_lustro, Lustro == 1960 | Lustro == 2010),
#   join_by(nuts_id == NUT) 
# ) -> pluto
# 
# 
# brks <- classInt::classIntervals(pluto$media, style = "pretty", n = 9)
# pluto <- pluto %>% mutate(pluto, Qtà = cut(media, breaks = brks$brks, include.lowest = TRUE))
# 
# ggplot() + 
#   geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
#   geom_sf(data = pluto, aes(fill = Qtà), na.rm = FALSE) + 
#   scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
#   # scale_fill_gradient(guide = guide_legend(title = "xxxx"), low = "#e3d662", high = "#918836", na.value = "transparent") +
#   facet_wrap(vars(Lustro, Quota) ) + theme_void()


# t(tab_durata_stagione) %>%
#   as_tibble() %>%
#   mutate(dplyr::across(c("ITC43", "ITC42")), lag)

# lag(t(tab_durata_stagione)) %>% round()


# t(tab_durata_stagione) %>% round() %>% View()

# 
# dplyr::select(df_durata, c(anno, nut, zs, durata)) %>%
#   set_names(c("Anno", "NUT", "Quota", "Durata")) %>%
#   filter(Quota >= 1200, NUT %in% prov_int$nuts_id) %>%
#   reshape2::melt(id.vars = c("Anno", "NUT", "Quota")) %>%
#   # filter(m_df, NUT == "ITC11") %>%
#   ggplot() +
#   geom_col(aes(Anno, value, fill = Quota), position = "dodge") +
#   geom_smooth(aes(Anno, value), method = "lm") +
#   facet_wrap(~NUT)

# dplyr::select(df_durata_media_lustro, c(Lustro, NUT, Quota, media)) %>%
  # filter(Quota >= 1500, NUT %in% prov_int$nuts_id) %>%
  # reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) %>%
  # ggplot() +
  # geom_col(aes(Lustro, value, fill = Quota), position = "dodge") +
  # geom_smooth(aes(Lustro, value), method = "lm", se = FALSE) +
  # facet_wrap(~NUT)

totale_innevamento <- function(nut) {
  nome_provincia(nut) -> provincia
  
  df <- dplyr::filter(m_df_machine_made, NUT == nut)
  if(nrow(df) == 0) {
    return()
  }
  
  df %>% 
    dplyr::mutate(Lustro = Anno - Anno %% 5) %>% 
    dplyr::group_by(NUT, Lustro, Quota) %>% 
    dplyr::summarise(totale = sum(value) )
}

# nut <- "ITF11"

map(prov_int$nuts_id, function(x) {
  totale_innevamento(x)
}) -> lst_machine_made_tot


lst_machine_made_tot <- vctrs::list_drop_empty(lst_machine_made_tot) # rimuovo gli elementi vuoti

do.call(rbind, lst_machine_made_tot) -> df_machine_made_lustro_tot # machine_made media nel lustro

map_machinemade_tot <- function() {
  inner_join(
    filter(nuts3, cntr_code == "IT") %>% dplyr::select(-c(the_geom)), 
    filter(df_machine_made_lustro_tot, Lustro %in% c(1960, 2010), Quota %in% seq(1600, 2800, by = 400)),
    join_by(nuts_id == NUT) 
  ) -> lustro_mm_tot_map
  
  brks <- classInt::classIntervals(lustro_mm_tot_map$totale, style = "fisher", n = 6)
  lustro_mm_tot_map <- lustro_mm_tot_map %>% 
    mutate(lustro_mm_tot_map, Artificiale = cut(totale, breaks = brks$brks, include.lowest = TRUE))
  
  ggplot() + 
    geom_sf(data = filter(nuts3, cntr_code == "IT"), color = "grey50", fill = "transparent") +
    geom_sf(data = lustro_mm_tot_map, aes(fill = Artificiale), na.rm = FALSE) + 
    scale_fill_brewer(palette = "Blues", na.value = "white") +
    facet_wrap(vars(Lustro) ) + 
    ggthemes::theme_map() + 
    ggtitle("Artificiale Kg/m²") + 
    facet_grid(vars(Lustro), vars(Quota)) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right", 
          legend.title = element_blank())
}
map_machinemade_tot()
