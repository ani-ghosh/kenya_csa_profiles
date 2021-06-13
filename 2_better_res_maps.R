library(tidyverse)
library(cowplot)
library(fst)
library(ggthemes)
library(raster)
library(tmap)
library(tmaptools)

# library(showtext)
# library(extrafont)
# # font_import()
# loadfonts(device = "win")

fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
}  




makeCountyPlot <- function(county, ff, datadir){
  cat("Plotting", county, "\n")
  
  # process spatial data
  v1 <- v[v$NAME_1 == county,]
  r1 <- crop(r, v1)
  r1 <- mask(r1, v1)
  
  x <- read.fst(file.path(datadir, "Vihiga_2021_2045_corrected.fst"))
  
  x1 <- x %>%
    dplyr::select(-c(ISO3, Country, county)) %>%
    dplyr::filter(season == "s1") %>%
    group_by(id, season) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    dplyr::select(-c(id, season, year))
  coordinates(x1) <- ~x+y
  gridded(x1) <- TRUE  
  r1x <- raster(x1)
  r1x <- extend(r1x, extent(v1)*1.25, value=NA)
  r11 <- focal(r1x, w = matrix(1,3,3), fun = fill.na, 
              pad = TRUE, na.rm = FALSE )
  r2x <- cover(r1x, r11)
  r2x <- resample(r2x, r1)
  
  
  plot(r2x)
  plot(v1, add = T)
  # unique latlonid
  # ill <- x[,c("id", "x", "y")] %>% filter(duplicated(.))
  
  # https://github.com/CIAT-DAPA/GIZ-climate-hazards/blob/2e7fac81eea636d5c5974db3bb04ff5d5706d47d/climate_scripts/09_graphs_do_maps.R
  # https://github.com/CIAT-DAPA/GIZ-climate-hazards/blob/2e7fac81eea636d5c5974db3bb04ff5d5706d47d/climate_scripts/10_do_elevation_map.R
  
  f <- grep(county, ff, value = TRUE)
  f1 <- grep("past", f, value = TRUE) 
  
  d1 <- read.fst(f1)
  
  # summarize data
  d1 <- d1 %>% 
    group_by(id, season) %>%
    summarise_all(mean, na.rm = TRUE)
  
  d1s1 <- d1[d1$season == "s1",]
  d1s1 <- merge(x, d1s1, by = "id")
  
  # future barplots
  # f2 <- grep("future", f, value = TRUE) 
  d2 <- lapply(f, read.fst)
  d2 <- bind_rows(d2) 
  
  # summarize data
  d2 <- d2 %>% 
    select(-id) %>%
    # mutate(era = ifelse(year < 2040, "2030", "2050")) %>%
    mutate(era = case_when(
      year <= 2015 ~ "1985-2015",
      year > 2015 & year <= 2041 ~ "2020-2040",
      year > 2040 & year <= 2070 ~ "2041-2060")) %>%
    group_by(era, season) %>%
    summarise_all(c("mean", "sd"), na.rm = TRUE) %>%
    mutate(season = case_when(
      season == "s1" ~ "Long Rain",
      season == "s2" ~ "Short Rain"))
  
  d2$era <- factor(d2$era, levels = c('1985-2015', '2020-2040', '2041-2060'))
  
  trange <- round(c(min(d2$AMT_mean) - 2 , max(d2$AMT_mean) + 2))
  trange <- ifelse(trange < 0, 0, trange)
  
  # past temeprature pattern
  plot1 <- ggplot(d1, aes(year, AMT_mean, col = season)) +
    geom_line(size = 1) +
    geom_smooth(method=lm, se=FALSE, linetype="dashed") +
    geom_point() +
    scale_y_continuous(limits = trange) +
    labs(x = "Year", y = expression("Temperature "(~degree~C))) +
    theme_classic() +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = c(.01, .99),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6)
    ) +
    # geom_ribbon(aes(ymax = AMT_mean + AMT_sd, ymin = AMT_mean - AMT_sd, fill = season), alpha = 0.3, colour=NA) +
    scale_fill_manual(values = c("#fecc5c","#df65b0"), aesthetics = c("color", "fill")) + 
    # scale_x_continuous(labels = sort(unique(d1$year)), breaks = sort(unique(d1$year)))
    scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))
  
  
  plot2 <- ggplot(d2, aes(x=as.factor(season), y=AMT_mean, fill=interaction(season, era))) +
    geom_bar(position=position_dodge(), stat="identity", width = 0.8) +
    coord_cartesian(ylim = trange) +
    # geom_errorbar(aes(ymax = AMT_mean + AMT_sd, ymin = AMT_mean - AMT_sd), width=.2, position=position_dodge(.8)) +
    theme_classic() +
    scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#e31a1c","#df65b0", "#dd1c77", "#980043"), 
                      aesthetics = c("color", "fill"),
                      breaks = c("Long Rain.1985-2015", "Long Rain.2020-2040", "Long Rain.2041-2060", 
                                 "Short Rain.1985-2015","Short Rain.2020-2040", "Short Rain.2041-2060")) +
    geom_text(aes(label = era, y = trange[1]), size = 4, 
              position = position_dodge(0.8),
              hjust = 0, vjust = 0.5, angle = 90) +
    theme(text = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size = 16),
          # axis.title.x = element_text(size = 16),
          # axis.text.x = element_text(size = 12),
          # axis.text.y = element_text(size = 12),
          # legend.text = element_text(size=12),
          # legend.title = element_blank(),
          # legend.position = c(.01, .99),
          # legend.justification = c("left", "top"),
          # legend.box.just = "left",
          # legend.margin = margin(6, 6, 6, 6),
          # legend.direction="horizontal",
          legend.position = "none")
  
  
  titletext <- ggdraw() + draw_label("Annual Mean Temperature Trends", fontface='bold', size = 20)
  pp1 <- plot_grid(plot1, plot2, rel_widths = c(2, 1.2), align = "h")
  pp1 <- plot_grid(titletext, pp1, ncol = 1, rel_heights=c(0.1, 1))
  
  save_plot(file.path(datadir, "plots", paste0(county, "_temperature_plot.png")), pp1, base_height = 5, base_width = 12)
  
  
  ######################################################################################################
  prange <- round(c(min(d2$ATR_mean-d2$ATR_sd) - 100 , max(d2$ATR_mean+d2$ATR_sd) + 100))
  prange <- ifelse(prange < 0, 0, prange)
  
  # past rainfall pattern
  plot3 <- ggplot(d1, aes(year, ATR_mean, col = season)) +
    geom_line(size = 1) +
    geom_smooth(method=lm, se=FALSE, linetype="dashed") +
    geom_point() +
    scale_y_continuous(limits = prange) +
    labs(x = "Year", y = "Rainfall (mm)") +
    theme_classic() +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = c(.01, .99),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6)
    ) +
    # geom_ribbon(aes(ymax = ATR_mean + ATR_sd, ymin = ATR_mean - ATR_sd, fill = season), alpha = 0.3, colour=NA) +
    scale_fill_manual(values = c("#a6bddb", "#66c2a4"), aesthetics = c("color", "fill")) + 
    # scale_x_continuous(labels = sort(unique(d1$year)), breaks = sort(unique(d1$year)))
    scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))
  
  
  plot4 <- ggplot(d2, aes(x=as.factor(season), y=ATR_mean, fill=interaction(season, era))) +
    geom_bar(position=position_dodge(), stat="identity", width = 0.8) +
    coord_cartesian(ylim = prange) +
    theme_classic() +
    # geom_errorbar(aes(ymax = ATR_mean + ATR_sd, ymin = ATR_mean - ATR_sd), width=.2, position=position_dodge(.8)) +
    scale_fill_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#66c2a4", "#41ae76", "#238b45"), 
                      aesthetics = c("color", "fill"),
                      breaks = c("Long Rain.1985-2015", "Long Rain.2020-2040", "Long Rain.2041-2060", 
                                 "Short Rain.1985-2015","Short Rain.2020-2040", "Short Rain.2041-2060")) +
    geom_text(aes(label = era, y = prange[1]), size = 4, 
              position = position_dodge(0.8),
              hjust = 0, vjust = 0.5, angle = 90) +
    theme(text = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size = 16),
          # axis.title.x = element_text(size = 16),
          # axis.text.x = element_text(size = 12),
          # axis.text.y = element_text(size = 12),
          # legend.text = element_text(size=12),
          # legend.title = element_blank(),
          # legend.position = c(.01, .99),
          # legend.justification = c("left", "top"),
          # legend.box.just = "left",
          # legend.margin = margin(6, 6, 6, 6),
          # legend.direction="horizontal",
          legend.position = "none")
  
  raintitle <- ggdraw() + draw_label("Annual Total Rainfall Trends", fontface='bold', size = 20)
  pp2 <- plot_grid(plot3, plot4, rel_widths = c(2, 1.2), align = "h")
  pp2 <- plot_grid(raintitle, pp2, ncol = 1, rel_heights=c(0.1, 1))
  
  save_plot(file.path(datadir, "plots", paste0(county, "_rainfall_plot.png")), pp2, base_height = 5, base_width = 12)
  
}


datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/kenya_counties/data"

ff <- list.files(datadir, pattern = "_corrected.fst$", full.names = T, recursive = T)

r <- getData('alt', country='KEN', path = datadir)
v <- getData('GADM', country='KEN', level=1, path = datadir)

# make line plots for past
countylist <- unique(sapply(strsplit(basename(ff), "_"), "[[", 1))