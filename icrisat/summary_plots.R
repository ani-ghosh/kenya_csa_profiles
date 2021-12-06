# plots for ICIRSAT regions
library(tidyverse)
library(cowplot)
library(fst)
library(ggthemes)
library(raster)
# library(showtext)
# library(extrafont)
# # font_import()
# loadfonts(device = "win")
library(terra)
getSummary <- function(era, f, v){
  f1 <- grep(era, f, value = TRUE)
  r <- lapply(f1, function(x){x <- rast(x); subset(x, c(1,12))})
  r <- rast(r)
  r <- crop(r, v)
  r <- mask(r,v)
  m <- tapp(r, index = c(1,2), fun = mean, na.rm = TRUE)
  d <- global(m, na.rm = TRUE)
  d <- t(d)
  colnames(d) <- c("AMT", "ATR")
  return(data.frame(country =  v$NAME_0, region = v$NAME_1, era = era, d, row.names = F))
}

# first get the summary AMT/ATR values from CMIP6
getSummaryCMIP6 <- function(i, adm, ff){
  ad <- adm[i,]
  datadir <- "C:/Users/anibi/Documents/ciat/cmip6/icrisat"
  v0 <- geodata::gadm(country = ad$iso3, level = 1, path = datadir)
  v <- v0[grep(ad$region,v0$NAME_1),]
  
  # prep raster files
  f <- grep(ad$iso3, ff, value = TRUE)
  eras <- c("2021-2040", "2041-2060", "2061-2080")
  
  tp <- lapply(eras, getSummary, f, v)
  tp <- data.table::rbindlist(tp, fill = TRUE)
  
  wc <- geodata::worldclim_global(var = "bio", res = 2.5, path = datadir)
  wc <- subset(wc, c(1,12))
  wcs <- crop(wc, v)
  wcs <- mask(wcs, v)
  wd <- global(wcs, na.rm = TRUE)
  wd <- t(wd)
  colnames(wd) <- c("AMT", "ATR")
  wd <- data.frame(country =  v$NAME_0, region = v$NAME_1, era = "historical", wd, row.names = NULL)
  tp <- rbind(wd, tp)
  return(tp)
}

makeCountyPlot <- function(i, adm, ff){
  ad <- adm[i,]
  datadir <- "C:/Users/anibi/Documents/ciat/cmip6/icrisat"
  
  # past trends
  ht <- read.csv("icrisat/Temperature-Air-2m-Mean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
  hp <- read.csv("icrisat/precipitation_flux.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
  d1 <- merge(ht, hp, by = c("adm", "year"))
  names(d1)[3:4] <- c("AMT", "ATR")
  d1$AMT <- d1$AMT - 273.15

  # future
  d2 <- getSummaryCMIP6(i, adm, ff) 
  
  # summarize data
  d2$era <- factor(d2$era, levels = c("historical","2021-2040", "2041-2060", "2061-2080"))
  
  trange <- round(c(min(d2$AMT) - 2 , max(d2$AMT) + 2))
  trange <- ifelse(trange < 0, 0, trange)
  
  # past temeprature pattern
  d1 <- d1[grep(ad$region, d1$adm),]
  
  plot1 <- ggplot(d1, aes(year, AMT)) +
    geom_line(size = 1, color = "#fecc5c") +
    geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "#fecc5c") +
    geom_point(color = "#fecc5c") +
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
    scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))
  
  plot2 <- ggplot(d2, aes(x = era, y=AMT, fill=era)) +
    geom_bar(position=position_dodge(), stat="identity", width = 0.8) +
    coord_cartesian(ylim = trange) +
    # geom_errorbar(aes(ymax = AMT_mean + AMT_sd, ymin = AMT_mean - AMT_sd), width=.2, position=position_dodge(.8)) +
    theme_classic() +
    scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#e31a1c", "#bd0026"), 
                      aesthetics = c("color", "fill"),
                      breaks = c("historical","2021-2040", "2041-2060", "2061-2080")) +
    theme(text = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  
  titletext <- ggdraw() + draw_label("Annual Mean Temperature Trends", fontface='bold', size = 20)
  pp1 <- plot_grid(plot1, plot2, rel_widths = c(2, 1.2), align = "h")
  pp1 <- plot_grid(titletext, pp1, ncol = 1, rel_heights=c(0.1, 1))
  
  save_plot(file.path(datadir, paste0(ad$region, "_temperature_plot.png")), pp1, base_height = 5, base_width = 12)
  
  
  ######################################################################################################
  prange <- round(c(min(d2$ATR) - 300 , max(d2$ATR) + 300))
  prange <- ifelse(prange < 0, 0, prange)
  
  # past rainfall pattern
  plot3 <- ggplot(d1, aes(year, ATR)) +
    geom_line(size = 1, color = "#a6bddb") +
    geom_smooth(method=lm, se=FALSE, linetype="dashed", color = "#a6bddb") +
    geom_point(color = "#a6bddb") +
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
    scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))
  
  plot4 <- ggplot(d2, aes(x=era, y=ATR, fill=era)) +
    geom_bar(position=position_dodge(), stat="identity", width = 0.8) +
    coord_cartesian(ylim = prange) +
    theme_classic() +
    # geom_errorbar(aes(ymax = ATR_mean + ATR_sd, ymin = ATR_mean - ATR_sd), width=.2, position=position_dodge(.8)) +
    scale_fill_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#045a8d"), 
                      aesthetics = c("color", "fill"),
                      breaks = c("historical","2021-2040", "2041-2060", "2061-2080")) +
    theme(text = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  raintitle <- ggdraw() + draw_label("Annual Total Rainfall Trends", fontface='bold', size = 20)
  pp2 <- plot_grid(plot3, plot4, rel_widths = c(2, 1.2), align = "h")
  pp2 <- plot_grid(raintitle, pp2, ncol = 1, rel_heights=c(0.1, 1))
  
  save_plot(file.path(datadir, paste0(ad$region, "_rainfall_plot.png")), pp2, base_height = 5, base_width = 12)
  
}



# input
adm <- data.frame(iso3 = c("MLI", "NER", "SEN"), region = c("Ségou","Tillabéry","Kaffrine"))

cdir <- "C:/Users/anibi/Documents/ciat/cmip6/icrisat/2.5"

ff <- list.files(cdir, glob2rx(paste0("*bioc*ssp585*.tif")), full.names = TRUE)

lapply(1:nrow(adm), makeCountyPlot, adm, ff)
