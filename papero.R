# init ####
{

  library(RColorBrewer)
  library(chron)
  library(dplyr)
  library(ggplot2) # package for plotting
  library(glue)
  library(kableExtra)
  library(mgcv)
  library(modifiedmk)
  library(modelsummary)
  library(ncdf4) # package for netcdf manipulation
  library(openxlsx)
  library(pander)
  library(purrr)
  library(readODS)
  library(readr)
  library(reshape2)
  library(gratia)
  # library(rgdal) # package for geospatial analysis
  library(sf)
  library(tibble)
  library(trend)
  
  confini <- st_read("~/R/turismo/shape/NUTS3_ID.shp")
  
  confini <- filter(confini, cntr_code == "IT")
  confini <- confini %>% 
    mutate(nuts_name = ifelse(nuts_id=="ITC20", "Valle d'Aosta", nuts_name) )
  
  prov_int <- confini$nuts_id

  anagrafica <- confini %>% select(nuts_id, nuts_name) %>% st_drop_geometry()
  
  m_df_durata <- readRDS("~/R/turismo/rds/m_df_durata.RDS")
  df_durata_media_lustro <- readRDS("~/R/turismo/rds/df_durata_media_lustro.RDS")
  df_durata_lustro <- readRDS("~/R/turismo/rds/df_durata_lustro.RDS")
  df_durata <- readRDS("~/R/turismo/rds/df_durata.RDS")

  theme_turismo <- function (base_size = 10, base_family = "Arial", base_line_size = 0.25, ...)  {
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
      axis.ticks = element_line(colour = "white", linewidth = base_line_size), 
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
      # panel.grid.major = element_line(colour = "white", linewidth = 0.3),
      # panel.grid.minor = element_blank(),
      strip.text = element_text(size = base_size), 
      # strip.background = element_rect(fill = NA, colour = "black", linewidth = 0.125), 
      strip.text.x = element_text(face = "bold", hjust = 0.03, margin = ggplot2::margin(b = small_size/2, t = small_size/4)), 
      strip.text.y = element_text(angle = -90, face = "bold", margin = ggplot2::margin(l = small_size/2, r = small_size/4)), 
      plot.margin = unit(c(5, 5, 0, 0), "pt"), 
      # plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1 * base_size, margin = ggplot2::margin(b = half_line), hjust = 0))
  }
  
  # prov nord ####
  confini %>%
    filter(nuts_id %in% grep("ITC|ITH", confini$nuts_id, value = TRUE)) %>% 
    select(nuts_id, nuts_name) %>% st_drop_geometry() -> prov_nord

  # confini %>% 
  #   filter(nuts_id %in% grep("ITC|ITH", confini$nuts_id, value = TRUE)) %>%
  #   ggplot() + geom_sf() + geom_sf_label(aes(label = nuts_id))
  
  # prov centro ####
  confini %>% 
    filter(cntr_code == "IT") %>% 
    filter(nuts_id %in% grep("ITI1|ITI2|ITI3|ITI4|ITF1|ITF2", confini$nuts_id, value = TRUE)) %>% 
    select(nuts_id, nuts_name) %>% st_drop_geometry() -> prov_centro
  
  # confini %>%
  #   filter(nuts_id %in% grep("ITI1|ITI2|ITI3|ITI4|ITF1|ITF2", confini$nuts_id, value = TRUE)) %>%
  #   ggplot() + geom_sf() + geom_sf_label(aes(label = nuts_id))
  
  # prov sud ####
  confini %>% 
    filter(cntr_code == "IT") %>% 
    filter(nuts_id %in% grep("ITF3|ITF4|ITF5|ITF6|ITG", confini$nuts_id, value = TRUE)) %>%
    select(nuts_id, nuts_name) %>% st_drop_geometry() -> prov_sud
  
  # saveRDS(prov_nord, file = "rds/prov_nord.RDS")
  # saveRDS(prov_centro, file = "rds/prov_centro.RDS")
  # saveRDS(prov_sud, file = "rds/prov_sud.RDS")
  
  # confini %>% 
  #   filter(nuts_id %in% grep("ITF3|ITF4|ITF5|ITF6|ITG", confini$nuts_id, value = TRUE)) %>%
  #   ggplot() + geom_sf() + geom_sf_label(aes(label = nuts_id))
  
  prov_papero <- c(
    "ITC13", # biella
    "ITC12", # vercelli
    "ITC11", # torino
    "ITC20", # vdaosta
    "ITH10", # bolzano
    "ITH20", # trento
    "ITC47", # brescia
    "ITC16", # cuneo
    "ITC44", # sondrio
    "ITH33" # belluno
  )
  # saveRDS(prov_papero, file = "rds/prov_papero.RDS")
}

nome_provincia <- function(nut) {
  filter(confini, nuts_id == nut) %>% 
    st_drop_geometry() %>% 
    dplyr::select(nuts_name) %>% as.character() -> provincia
  
  return(provincia)
}

# https://ambientenonsolo.com/le-aree-di-alta-montagna-si-scaldano-piu-rapidamente-del-resto-del-globo/

graf_durata <- function(nut, quote) {
  provincia <- nome_provincia(nut)
  
  df <- filter(m_df_durata, NUT == nut, Quota %in% quote)
  if(nrow(df) == 0) 
    return("")
  
  df %>% 
    ggplot(aes(Anno, value)) + 
    geom_step() + 
    geom_smooth(method = "gam", formula = y ~ s(x, k=3) ) +
    facet_wrap(~Quota) + ylab("") +
    scale_y_continuous(breaks = seq(0, 250, by = 40)) +
    theme(
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none") +
    theme_turismo() +
    ggtitle(nome_provincia(nut)) -> g0
  
  ggsave(g0, filename = glue("immagini/durata_{provincia}_quote_gam.jpg"), 
         width = 18, height = 14, units = "cm", dpi = 600)
  
  df %>%
    group_by(Anno, NUT) %>%
    summarise(mm = mean(value)) %>%
    ggplot(aes(Anno, mm)) +
    geom_step() + 
    ylab("Duration of snow season (days)") + xlab("Year") +
    scale_y_continuous(breaks = seq(0, 250, by = 20)) +
    theme(
      # axis.text.x = element_blank(), 
      # axis.text.y = element_blank(),
      legend.position = "none") +
    theme_turismo() +
    geom_smooth(method = "gam", formula = y ~ s(x, k=3) ) -> g1

    ggsave(g1, filename = glue("immagini/durata_{provincia}_gam.jpg"), 
         width = 16, height = 12, units = "cm", dpi = 600)  
  # lay <- rbind(c(1,1),
  #              c(1,1),
  #              c(2,2))
  
  # gridExtra::grid.arrange(g0, g1, layout_matrix = lay, 
  #                         left = "Duration of snow season (days)" )
}

# graf_durata("ITC44", seq(1000, 3000, by = 400))
# graf_durata("ITC13", seq(1000, 3000, by = 400))

graf_durata_lustro <- function(nuts, quote = c()) {
  # provincia <- nome_provincia(nut)
  df <- filter(df_durata_media_lustro, NUT %in% nuts)
  
  inner_join(df, prov_nord, join_by("NUT" == "nuts_id")) -> df
  # , Quota %in% quote
  
  # reshape2::melt(df, id.vars = c("NUT", "Lustro", "Quota")) %>%  
  #   ggplot(aes(Lustro, value)) + 
  #   geom_step() + 
  #   geom_smooth(method = lm, se = FALSE) +
  #   facet_wrap(~Quota) + ylab("Durata stagione (gg)") +
  #   theme(
  #     axis.text.x = element_text(angle = 45, hjust = 1),
  #     legend.position = "none") + theme_turismo() +
  #   ggtitle(nome_provincia(nut)) -> g0
  
  reshape2::melt(df, id.vars = c("NUT", "nuts_name", "Lustro", "Quota")) %>%
    group_by(Lustro, nuts_name) %>%
    summarise(mm = mean(value)) %>%
    ggplot(aes(Lustro, mm)) +
    geom_step() + 
    facet_wrap(~(nuts_name)) +
    ylab("Duration of snow season (days)") + xlab("Five years period") +
    theme_turismo() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) 
}
# graf_durata_lustro(c("ITC13", "ITH10", "ITC12", "ITC11", "ITC44", "ITC47")) %>% 
#   ggsave(filename = "immagini/durata_province.jpg", width = 15, height = 12, units = "cm", dpi = 600)

graf_durata_lustro_quote <- function(nuts, quote = c()) {
  # provincia <- nome_provincia(nut)
  df <- filter(df_durata_media_lustro, NUT %in% nuts, Quota %in% quote)
  
  inner_join(df, prov_nord, join_by("NUT" == "nuts_id")) -> df
  
  df <- df %>% mutate(Quota = paste(Quota, "mt")) 
  
  reshape2::melt(df, id.vars = c("NUT", "nuts_name", "Lustro", "Quota")) %>%
    ggplot(aes(Lustro, value)) +
    geom_step() +
    geom_smooth(method = gam, formula = y ~ s(x, k = 3), se = TRUE) +
    facet_wrap(~Quota) + 
    scale_y_continuous(breaks = seq(0, 200, by = 20)) +
    ylab("Duration of snow season (days)") + xlab("Five years period") +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + #theme_classic() +
    theme_turismo() +
    ggtitle(nome_provincia(nuts))
}
# graf_durata_lustro_quote("ITC13", seq(1400, 1900, by = 100)) %>%
  # ggsave(filename = "immagini/durata_quote_biella.jpg", width = 15, height = 12, units = "cm", dpi = 600)

summ_lm_durata <- function(x, quote) {
  df_durata %>% 
    filter(nut == x, zs > 1800) -> df
  
  df$quota <- factor(df$zs)
  
  if(levels(df$quota) %>% length() > 1) {
    fit <- lm(durata ~ quota + anno, data = df)
    summary(fit)
  }
}
# summ_lm_durata("ITC47")

summ_gam_durata <- function(x, quote) {
  df_durata %>% 
    filter(nut == x, zs %in% quote) -> df

  tit <- nome_provincia(x)
  print(tit)
  
  df$quota <- factor(df$zs)
  if(nrow(df) > 0){
    if(levels(df$quota) %>% length() > 1) {
      fit <- gam(durata ~ s(anno, k = 6) + quota, data = df, method = "REML")
      
      p <- appraise(fit) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_check.jpg"), width = 8, height = 8)
      
      p <- draw(fit) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_splines.jpg"), width = 8, height = 8)
      
      p <- draw( evaluate_parametric_term(fit, "quota") ) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_parametric.jpg"), width = 8, height = 8)
      
      sink(file =  glue("testi/{tit}_diagn.txt") )
      
      basic_summary <- summary(fit) 
      print(basic_summary$p.table)
      print(basic_summary$s.table)
      
      # modelsummary(fit, output = "markdown", statistic = c("t = {statistic}", "se = {std.error}", "conf.int")) %>% print()
      cat("\n")
      modelsummary(fit, output = "markdown",
                   statistic = NULL,
                   estimate = "{estimate} [{conf.low}, {conf.high}] ({p.value} {stars})") %>% print()
      
      # modelsummary::modelsummary(fit, output = "markdown") %>% print()
      # modelsummary::modelsummary(fit,
      #                            estimate = c("{estimate} [{conf.low}, {conf.high}] ({p.value}){stars}"),
      #                            output = "markdown") %>% print()
      
      
      sink()  
    }
  }
}
# grafici durata annuale ####
{
  # summ_gam_durata("ITC13", seq(1000, 3000, by = 100)) # biella
  # summ_gam_durata("ITC12", seq(1000, 3000, by = 100)) # vercelli
  # summ_gam_durata("ITC11", seq(1000, 3000, by = 100)) # torino
  # summ_gam_durata("ITC20", seq(1000, 3000, by = 100)) # vdaosta
  # summ_gam_durata("ITH10", seq(1000, 3000, by = 100)) # bolzano
  # summ_gam_durata("ITH20", seq(1000, 3000, by = 100)) # trento
  # summ_gam_durata("ITC47", seq(1000, 3000, by = 100)) # brescia
  # summ_gam_durata("ITC16", seq(1000, 3000, by = 100)) # cuneo
  # summ_gam_durata("ITC44", seq(1000, 3000, by = 100)) # sondrio
  # summ_gam_durata("ITH33", seq(1000, 3000, by = 100)) # belluno
}

graf_durata_noquote <- function(nuts) {
  left_join(df_durata_lustro, anagrafica, join_by (NUT==nuts_id) ) %>%
    filter(NUT %in% nuts) %>% 
    group_by(Lustro, nuts_name) %>% mutate(Media = mean(media)) -> df
  
  select(df, c(Lustro, nuts_name, Media)) %>%  
    melt(id.vars = c("Lustro", "nuts_name" )) %>% 
    ggplot(aes(Lustro, value)) + geom_step() +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 6), se = TRUE) +
    xlab("Five years period") + ylab("Duration of snow season (average)") +
    scale_y_continuous(breaks = seq(50, 250, by = 25)) +
    facet_wrap(~nuts_name, ncol = 5) + theme_turismo() -> g 
  
    ggsave(g, filename = "immagini/top-five_province.jpg", width = 12, height = 7, dpi = 300)
}
graf_durata_noquote(c("ITC13", "ITH10", "ITC12", "ITC44", "ITC11"))



summ_gam_durata_lustro <- function(x, quote) {
  df_durata_media_lustro %>% 
    filter(NUT == x, Quota %in% quote) -> df
  
  tit <- nome_provincia(x)
  print(tit)
  df$quota <- factor(df$Quota)
  if(nrow(df) > 0){
    if(levels(df$quota) %>% length() > 1) {
      fit <- gam(media ~ quota + s(Lustro, k = 6), data = df, method = "REML")
      
      p <- appraise(fit) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_check_5.jpg"), width = 8, height = 8)
      
      p <- draw(fit) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_splines_5.jpg"), width = 8, height = 8)
      
      p <- draw( evaluate_parametric_term(fit, "quota") ) & theme_turismo()
      ggsave(p, filename = glue("immagini/{tit}_gam_parametric_5.jpg"), width = 8, height = 8)
      
      sink(file =  glue("testi/{tit}_diagn_5.txt") )
      
      basic_summary <- summary(fit) 
      print(basic_summary$p.table)
      print(basic_summary$s.table)
      # modelsummary::modelsummary(fit,
      #                            statistic = "p.value",
      #                            output = "markdown") %>% print()
      modelsummary::modelsummary(fit,
                                 estimate = c("{estimate} [{conf.low}, {conf.high}] ({p.value}){stars}"),
                                 output = "markdown") %>% print()
      

      sink()
    }
  }
}
# grafici durata lustro ####
{
  # summ_gam_durata_lustro("ITC13", seq(1000, 3000, by = 100)) # biella
  # summ_gam_durata_lustro("ITC12", seq(1000, 3000, by = 100)) # vercelli
  # summ_gam_durata_lustro("ITC11", seq(1000, 3000, by = 100)) # torino
  # summ_gam_durata_lustro("ITC20", seq(1000, 3000, by = 100)) # vdaosta
  # summ_gam_durata_lustro("ITH10", seq(1000, 3000, by = 100)) # bolzano
  # summ_gam_durata_lustro("ITH20", seq(1000, 3000, by = 100)) # trento
  # summ_gam_durata_lustro("ITC47", seq(1000, 3000, by = 100)) # brescia
  # summ_gam_durata_lustro("ITC16", seq(1000, 3000, by = 100)) # cuneo
  # summ_gam_durata_lustro("ITC44", seq(1000, 3000, by = 100)) # sondrio
  # summ_gam_durata_lustro("ITH33", seq(1000, 3000, by = 100)) # belluno
}

if(!exists("rsq_adj")) {
map(prov_papero, \(cod) { 
  df_durata_media_lustro %>% 
    filter(NUT == cod, Quota %in% seq(1000, 3000, by = 100)) %>% 
    mutate(Quota = factor(Quota) ) -> df
  
  tit <- nome_provincia(cod)
  fit <- gam(media ~ Quota + s(Lustro, k = 6), data = df, method = "REML")
  summary(fit)$r.sq %>% round(3)
}) -> models_lustro
names(models_lustro) <- prov_papero

do.call(rbind, models_lustro) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "nuts_id") %>% inner_join(prov_nord) -> rsq_adj 
  saveRDS(rsq_adj, file = "~/R/turismo/rds/rsq_adj.RDS")
}

# media sul quinquennio per provincia ####
if(!exists("df_durata_media_lustro")) {
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
  saveRDS(df_durata_media_lustro, file = "~/R/turismo/rds/df_durata_media_lustro.RDS" )
  
  df_durata_media_lustro %>% 
    filter(Quota >= 1000) %>% 
    reshape2::melt(id.vars = c("Lustro", "NUT", "Quota")) -> m_df_durata_lustro
  
  saveRDS(m_df_durata_lustro, file = "~/R/turismo/rds/m_df_durata_lustro.RDS")
}


map_durata <- function(prov_id, alts) {
  
  df_durata_lustro <- df_durata_lustro %>% mutate(Quota2 = paste(Quota, "mt")) 
  
  inner_join(
    confini %>% dplyr::select(-c(the_geom)), 
    filter(df_durata_lustro, Lustro %in% c(1960, 2010), Quota %in% alts),
    join_by(nuts_id == NUT) 
  ) -> lustro_dur_map
  
  lustro_dur_map <- filter(lustro_dur_map, nuts_id %in% prov_id$nuts_id)
  confini <- filter(confini, nuts_id %in% prov_id$nuts_id)
  
  brks <- classInt::classIntervals(lustro_dur_map$media, style = "pretty", n = 9)
  lustro_dur_map <- lustro_dur_map %>% 
    mutate(lustro_dur_map, Durata = cut(media, breaks = brks$brks, include.lowest = TRUE))
  
  ggplot() + 
    geom_sf(data = filter(confini, nuts_id %in% prov_id$nuts_id), color = "grey50", fill = "transparent") +
    geom_sf(data = lustro_dur_map, aes(fill = Durata), na.rm = FALSE) +
    scale_fill_brewer(palette = "Greens", na.value = "white", name = "Days") +
    facet_grid(vars(Lustro), vars(Quota2)) + 
    theme_void() +
    theme_bw(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right"
      ) +
    ggtitle("Duration of snow season") 
}
map_durata(prov_nord, seq(1000, 3800, by = 400)) %>%
  ggsave(filename = "immagini/durata_nord.jpg", width = 19, height = 6, units = "cm", dpi = 600)
map_durata(prov_centro, seq(1000, 3800, by = 400)) %>%
  ggsave(filename = "immagini/durata_centro.jpg", width = 19, height = 6, units = "cm", dpi = 600)
map_durata(prov_sud, seq(1000, 3800, by = 400)) %>%
  ggsave(filename = "immagini/durata_sud.jpg", width = 19, height = 6, units = "cm", dpi = 600)


# Mann-Kendall ####
if(!exists("df_mk_durata")) {
  map(c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id), \(id) {
    
    map(seq(1000, 3200, by = 200), \(z) {
      df_durata %>% 
        filter(nut == id, zs == z) %>% 
        dplyr::select(c("durata")) -> tmp
      
      if( nrow(tmp) > 0 ) {
        # mk.test( tmp$durata )
        mk.test( tmp$durata )$p.value %>% round(digits = 3)
        
      }else{
        NA
      }
    }) 
  }) -> ktest_durata
  
  names(ktest_durata) <- c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id)
  
  map(c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id), \(k) {
    ktest_durata[[k]] %>% set_names(seq(1000, 3200, by = 200))
  }) -> ole
  
  names(ole) <- c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id)
  # do.call(rbind, ole) %>% na.omit()
  
  
  do.call(rbind.data.frame, ktest_durata) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "nuts_id") %>% 
    set_names(c("nuts_id", paste0("mslm", seq(1000, 3200, by = 200)) )) %>% 
    inner_join(rbind(prov_nord, prov_centro, prov_sud)) -> df_mk_durata
  saveRDS(df_mk_durata, file = "rds/df_mk_durata.RDS")
}

# Mann-Kendall lustro ####
if(!exists("df_mk_durata_lustro")) {
  map(c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id), \(id) {
    
    map(seq(1000, 3200, by = 100), \(z) {
      df_durata_lustro %>% 
        filter(NUT == id, Quota == z) %>% 
        dplyr::select(c("media")) -> tmp
      
      if( nrow(tmp) > 0 ) {
        # mk.test( tmp$durata )
        mk.test( tmp$media )$p.value %>% round(digits = 3)
      }else{
        NA
      }
    }) 
  }) -> ktest_durata_lustro
  
  names(ktest_durata_lustro) <- c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id)
  
  map(c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id), \(k) {
    ktest_durata_lustro[[k]] %>% set_names(seq(1000, 3200, by = 100))
  }) -> ole_lustro
  
  names(ole_lustro) <- c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id)
  
  do.call(rbind.data.frame, ktest_durata_lustro) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "nuts_id") %>% 
    set_names(c("nuts_id", paste0("mslm", seq(1000, 3200, by = 100)) )) %>% 
    inner_join( rbind(prov_nord, prov_centro, prov_sud) ) -> df_mk_durata_lustro
  saveRDS(df_mk_durata_lustro, "rds/df_mk_durata_lustro.RDS")
  
  df_mk_durata_lustro %>% 
    filter(nuts_id %in% prov_papero) %>% 
    select(-nuts_id) -> df_mk_durata_lustro_tab
  
  names(df_mk_durata_lustro_tab)[1:12] <- paste( seq(1000, 3200, by = 100), "mt")
  df_mk_durata_lustro_tab %>% select(c(nuts_name, 1:12)) -> df_mk_durata_lustro_tab
  
  saveRDS(df_mk_durata_lustro_tab, file = "rds/df_mk_durata_lustro_tab.RDS")
}


# mann-kendall autocorr ####
if(!exists("df_mmk_durata_lustro")) {
  # map(c(prov_nord$nuts_id, prov_centro$nuts_id, prov_sud$nuts_id), \(id) {
  map(c(prov_nord$nuts_id), \(id) {
      
    map(seq(1000, 3200, by = 200), \(z) {
      df_durata_lustro %>% 
        filter(NUT == id, Quota == z) %>% 
        dplyr::select(c("media")) -> tmp
      
      if( nrow(tmp) > 0 ) {
        # d <- ts(tmp$media, start = 1960, end = 2010, frequency = 1)
        mmkh(tmp$media)[2]
      }else{
        NA
      }
    }) 
  }) -> mmktest_durata_lustro
  
  names(mmktest_durata_lustro) <- c(prov_nord$nuts_id)

  map(c(prov_nord$nuts_id), \(k) {
    mmktest_durata_lustro[[k]] %>% set_names(seq(1000, 3200, by = 200))
  }) -> ole_lustro

  names(ole_lustro) <- c(prov_nord$nuts_id)

  do.call(rbind.data.frame, mmktest_durata_lustro) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "nuts_id") %>%
    set_names(c("nuts_id", paste0("mslm", seq(1000, 3200, by = 200)) )) %>%
    inner_join( rbind(prov_nord, prov_centro, prov_sud) ) -> df_mmk_durata_lustro
  
  saveRDS(df_mk_durata_lustro, "rds/df_mmk_durata_lustro.RDS")
}
