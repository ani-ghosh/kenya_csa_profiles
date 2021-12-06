library(raster)
library(terra)
library(stars)
library(rasterVis)
library(tidyverse)
library(tmap)
library(tmaptools)
library(patchwork)
library(grid)
library(classInt)

# create a list for variable and palette
varCol <- data.frame(vars = c("CDD","P5D","P95","NT35","ndws","ATR","AMT","SLGP", "LGP"),
                     gradlow = c("#000099", "#A50026", "#A50026", "#000099", "#000099", 
                                 "#A50026", "#000099","#A50026", "#A50026"),
                     gradmid = rep("white", 9),
                     gradhigh = c("#A50026", "#000099", "#000099", "#A50026", "#A50026", 
                                  "#000099", "#A50026","#000099", "#000099"),
                     labs = c("CDD \n(days)", "P5D\n(mm/5 days)", "P95\n(mm/day)", "NT35 \n(days)",
                              "NDWS\n(days/month)", "ATR\n(mm/year)", "AMT\n(\u00B0C)",
                              "SLGP\n(Day of\nthe year)", "LGP\n(Day of\nthe year)"))
 
thememap <- function(...){
  theme_bw() + 
    theme(line = element_blank(),
          panel.spacing.x = unit(2, "lines"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 25),
          strip.text = element_text(size = 30),
          legend.position = 'bottom', 
          legend.justification="center",
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25, margin = margin(0, 5, 0, 5)),
          legend.margin = margin(0, 0, 0, 0),
          legend.title.align = 0.5,
          legend.spacing.x = unit(0.5, "in"),
          # legend.spacing = unit(5, units = 'cm'),
          # plot.title = element_text(hjust = 0.5),
          ...
    ) 
}


makeAbsMapRaster <- function(var, rr, varCol, vfort, cboundfort, xlims, ylims, ...){
  cat("Plotting", var, "\n")
  
  gs <- raster::subset(rr, grep(var, names(rr)))
  
  # color breaks and limits
  var <- varCol[varCol$vars == var,]
  mn <- min(cellStats(gs, 'min', na.rm = T))
  mx <- max(cellStats(gs, 'max', na.rm = T))          
  my_limits <- c(round(mn,0), round(mx, 0))
  
  n <- 7
  my_breaks <- round(seq(my_limits[1], my_limits[2],  length.out = n), 0)
  my_limits <- c(ifelse(my_limits[1] > my_breaks[1], my_breaks[1], my_limits[1]),
                 ifelse(my_limits[2] < my_breaks[n], my_breaks[n], my_limits[2]))
  
  # setup color palette
  if(var$vars %in% c("P5D","P95","ATR","SLGP", "LGP")){
    my_palette <- scale_fill_gradientn(limits =  my_limits, 
                                       breaks = unique(my_breaks),
                                       labels = unique(my_breaks),
                                       colours = blues9, 
                                       oob = scales::oob_squish,
                                       na.value = NA,
                                       guide = guide_colourbar(barwidth = 30, 
                                                               label.theme = element_text(angle = 25, size = 35)))
  } else if(var$vars == 'AMT'){
    my_palette <- scale_fill_gradient(limits =  my_limits, 
                                      breaks = unique(my_breaks),
                                      labels = unique(my_breaks),
                                      low = "yellow", high = "red",
                                      oob = scales::oob_squish,
                                      na.value = NA,
                                      guide = guide_colourbar(barwidth = 30,  
                                                              label.theme = element_text(angle = 25, size = 35)))
  } else {
    my_palette <- scale_fill_viridis_c(limits = my_limits, 
                                       breaks = unique(my_breaks),
                                       labels = unique(my_breaks),
                                       oob = scales::oob_squish,
                                       na.value = NA,
                                       guide = guide_colourbar(barwidth = 30, 
                                                               label.theme = element_text(angle = 25, size = 35))) 
  }
  
  names(gs) <- c("b1", "b2", "b3")
  
  absmap <- gplot(gs) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable, labeller = labeller(variable = 
                                                 c("b1" = "Historical",
                                                   "b2" = "2030",
                                                   "b3" = "2050"))) +
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.6), size = 0.75) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.2), size = 1.25) +
    coord_sf(xlim = xlims, ylim = ylims) +
    scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
    scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
    thememap() +
    labs(fill = var$labs, 
         x = 'Longitude', y = 'Latitude') +
    my_palette
  return(absmap)
}


# 2 panel map for showing changes
makeChangeMapRaster <- function(var, rr, varCol, vfort, cboundfort, xlims, ylims, ...){
  
  cat("Plotting", var, "\n")
  
  gs <- raster::subset(rr, grep(var, names(rr)))
  
  gs1 <- raster::subset(gs, grep("historical", names(gs)))
  gs2 <- raster::subset(gs, grep("2030", names(gs)))
  gs3 <- raster::subset(gs, grep("2050", names(gs)))
  
  # compute changes
  gs21 <- gs2 - gs1
  gs31 <- gs3 - gs1

  gs <- stack(gs21, gs31)
  names(gs) <- c("b1", "b2")
  
  # color breaks and limits
  var <- varCol[varCol$vars == var,]
  my_limits_MM <- c(min(cellStats(gs, 'min', na.rm = T)), max(cellStats(gs, 'max', na.rm = T)))
  # cint <- classIntervals(values(gs), 2, style = "quantile")
  # my_breaks_MM <- round(cint$brks, 0)
  my_breaks_MM <- round(seq(my_limits_MM[1], my_limits_MM[2],  length.out= 3), 0)
  my_limits_MM <- c(ifelse(my_limits_MM[1] > my_breaks_MM[1], my_breaks_MM[1], my_limits_MM[1]),
                    ifelse(my_limits_MM[2] < my_breaks_MM[3], my_breaks_MM[3], my_limits_MM[2]))
  
  # main plot
  chgmap <- gplot(gs) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable, labeller = labeller(variable = 
                                                 c("b1" = "Change 2030",
                                                   "b2" = "Change 2050"))) +
    # if want to include Delta "\u0394 2041-2060"
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.6), size = 0.75) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.2), size = 1.25) +
    coord_sf(xlim = xlims, ylim = ylims) +
    scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
    scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
    thememap() +
    labs(fill = paste0("Change in ", var$vars), 
         x = 'Longitude', y = 'Latitude') +
    scale_fill_gradient2(limits =  my_limits_MM, 
                         breaks = my_breaks_MM,
                         oob = scales::oob_squish,
                         na.value = NA,
                         low = var$gradlow, mid = var$gradmid, high = var$gradhigh, 
                         guide = guide_colourbar(barwidth = 25, 
                                                 label.theme = element_text(angle = 25, size = 35)))
  return(chgmap)
}


makePlotCounty <- function(var, county, varcol, v, rr, datadir){
  
  cat("Processing", county, "for", var, "\n")
  
  # county specific 
  cbound <- v[v$NAME_1 == county,]
  
  # check files
  cdir <- file.path(datadir, "combined_data/interim", county)
  dir.create(cdir, F, T)
  ofile <- file.path(cdir, paste0(county, "_", var, ".tif"))
  # unlink(ofile)
  if(!file.exists(ofile)){
    gs <- crop(rr, cbound, snap = "in")
    gs <- mask(gs, cbound, filename = ofile, overwrite = TRUE)
  } 
  gs <- stack(ofile)
  names(gs) <- apply(expand.grid(c("AMT_", "ATR_"), c("historical", "2030", "2050")), 1, paste, collapse="")
  # boundaries
  xlims <- c(xmin(cbound), xmax(cbound))
  ylims <- c(ymin(cbound), ymax(cbound))
  
  # fortify for ggplot
  vfort <- fortify(v)
  cboundfort <- fortify(cbound)
  
  # maps
  p1 <- makeAbsMapRaster(var, gs,  varCol, vfort, cboundfort, xlims, ylims)
  p2 <- makeChangeMapRaster(var, gs,  varCol, vfort, cboundfort, xlims, ylims)
  
  # merge 
  pp <- p1 + theme(plot.margin = unit(c(0.5,0,1,0), "in")) +
    p2 + 
    plot_annotation(title = paste(county, var),
                    caption = 'Alliance of Bioversity and CIAT',
                    theme = theme(plot.title = element_text(size = 50))) +
    plot_layout(nrow = 2, heights = c(0.55, 0.45))
  
  pdir <- file.path(datadir, "combined_data/finalplots", county)
  dir.create(pdir, F, T)
  
  ggsave(file.path(pdir, paste0(county,"_", var, ".png")), pp, device = "png",
         width = 18, height = 24, units = "in")
}

####################################################################################################
iso <- "NGA"
# datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/PAK/CMIP6/baloch/"
datadir <- "C:/Users/anibi/Documents/ciat/cmip6"
# ff <- list.files(datadir, pattern = "^NGA.*tif$", full.names = T, recursive = T)
ff <- list.files(datadir, pattern = glob2rx(paste("NGA","tif",sep="*")), 
                 full.names = T, recursive = T)

cur <- rast(grep("bio_1_12", ff, value = TRUE))

gcm <- c("ACCESS-CM2", "INM-CM5-0", "HadGEM3-GC31-LL", "IPSL-CM6A-LR", "CNRM-CM6-1-HR")
gcms <- paste(gcm, collapse = "|")
fut <- grep(gcms, ff, value = TRUE)
# create a stack of historical and future variables

# mean of 2030
fut1 <- rast(grep("2021-2040", fut, value = TRUE))
fut1 <-  subset(fut1, grep("wc2_1$|wc2_12$", names(fut1)))
fut1 <- tapp(fut1, index = c(1,2), fun = mean, na.rm = TRUE)

# mean of 2050
fut2 <- rast(grep("2041-2060", ff, value = TRUE))
fut2 <-  subset(fut2, grep("wc2_1$|wc2_12$", names(fut2)))
fut2 <- tapp(fut2, index = c(1,2), fun = mean, na.rm = TRUE)

# combine all and rename
rr <- c(cur, fut1, fut2)
names(rr) <- apply(expand.grid(c("AMT_", "ATR_"), c("historical", "2030", "2050")), 1, paste, collapse="")
writeRaster(rr, file.path(datadir, paste0(iso, "_all_bio1_bio12.tif")), overwrite = TRUE)

# admin boundaries
vg <- getData('GADM', country='NGA', level=1, path = datadir)
clist <- c("Adamawa","Borno","Yobe")

# subset climate data by roi
rr <- stack(file.path(datadir, paste0(iso, "_all_bio1_bio12.tif")))

for(county in clist){
  {
    for (var in c("AMT", "ATR")){
      makePlotCounty(var, county, varcol, v, rr, datadir) 
    }
  }
}

#############################################################################################
# AMT from future models
# getVar <- function(f, var){
#   r <- rast(f)
#   r <- subset(r, grep(var, names(r)))
#   nm <- gsub("NGA_wc2.1_30s_bioc_|_ssp585_2021-2040.tif","", basename(f))
#   names(r) <- paste0(nm, "_", var)
#   return(r)
# }

# elevation plot
makeElevationPlot <- function(county, v, elev, datadir){
  # county boundary
  cat("Processing", county, "\n")
  cbound <- v[v$NAME_1 == county,]
  celev <- crop(elev, cbound)
  celev <- mask(celev, cbound)
  
  # boundaries
  xlims <- c(xmin(cbound), xmax(cbound))
  ylims <- c(ymin(cbound), ymax(cbound))
  
  # fortify for ggplot
  vfort <- fortify(v)
  cboundfort <- fortify(cbound)
  
  # color breaks and limits
  mv <- min(cellStats(celev, 'min', na.rm = T))
  mx <- max(cellStats(celev, 'max', na.rm = T))          
  my_limits <- c(round(mv*0.9,0), round(mx*1.1, 0))
  my_breaks <- round(seq(my_limits[1], my_limits[2],  length.out = 3), 0)
  my_limits <- c(ifelse(my_limits[1] > my_breaks[1], my_breaks[1], my_limits[1]),
                 ifelse(my_limits[2] < my_breaks[3], my_breaks[3], my_limits[2]))
  
  
  emap <- gplot(celev) + geom_tile(aes(fill = value)) +
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.8), size = 1) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.1), size = 2) +
    coord_sf(xlim = xlims, ylim = ylims) +
    scale_fill_gradientn(colours = rev(terrain.colors(20)),
                         na.value =  NA,
                         limits = my_limits, 
                         breaks = unique(my_breaks),
                         labels = unique(my_breaks),
                         guide = guide_colourbar(barwidth = 15, barheight = 3,
                                                 label.theme = element_text(angle = 25, size = 25))) +
    scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
    scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
    labs(fill = "Elevation \n(m)",x = 'Longitude', y = 'Latitude') + # title = county ,
    theme_bw() + 
    theme(line = element_blank(),
          panel.spacing.x = unit(2, "lines"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 25),
          strip.text = element_text(size = 30),
          legend.position = 'bottom', 
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25),
          legend.spacing = unit(5, units = 'cm'),
          legend.spacing.x = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.1))
  
  pdir <- file.path(datadir, "combined_data/finalplots", county)
  dir.create(pdir, F, T)
  
  ggsave(file.path(pdir, paste0(county,"_elevation.png")), emap, device = "png",
         width = 12, height = 12, units = "in")
}

elev <- stack(file.path(datadir, "NGA_msk_alt.grd"))

for(county in clist){
  makeElevationPlot(county, vg, elev, datadir)
}


#######################################################################################################
iso <- "NGA"

datadir <- "C:/Users/anibi/Documents/ciat/cmip6"
# admin boundaries
vg <- getData('GADM', country='NGA', level=1, path = datadir)
clist <- c("Adamawa","Borno","Yobe")

# subset climate data by roi
rr <- rast(file.path(datadir, paste0(iso, "_all_bio1_bio12.tif")))

# summary statistics
vt <- vect(vg)
vt <- vt[vt$NAME_1 %in% clist,]
dd <- extract(rr, vt, mean, na.rm = TRUE)
dd$ID <- NULL
dd <- data.frame(district = vt$NAME_1, dd)
write.csv(dd, file.path(datadir, "summary_stat_bio1_12.csv"), row.names = FALSE)


#########################################################################################################
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
adm <- data.frame(iso3 = c("NGA"), region = c("Adamawa","Borno","Yobe"))

cdir <- "C:/Users/anibi/Documents/ciat/cmip6/icrisat/2.5"

ff <- list.files(cdir, glob2rx(paste0("*bioc*ssp585*.tif")), full.names = TRUE)

lapply(1:nrow(adm), makeCountyPlot, adm, ff)
