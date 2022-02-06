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


makeAbsMapRaster <- function(var, rr, varCol, vfort, xlims, ylims, ...){
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
makeChangeMapRaster <- function(var, rr, varCol, vfort, xlims, ylims, ...){
  
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


makePlotCountry <- function(var, varcol, vg, rr, datadir, iso){
  
   # boundaries
  xlims <- c(xmin(vg), xmax(vg))
  ylims <- c(ymin(vg), ymax(vg))
  
  # fortify for ggplot
  vfort <- fortify(vg)
  
  # maps
  p1 <- makeAbsMapRaster(var, rr,  varCol, vfort,  xlims, ylims)
  p2 <- makeChangeMapRaster(var, rr, varCol, vfort, xlims, ylims)
  
  # merge 
  pp <- p1 + theme(plot.margin = unit(c(0.5,0,1,0), "in")) +
    p2 + 
    plot_annotation(title = paste(iso, var),
                    caption = 'Alliance of Bioversity and CIAT',
                    theme = theme(plot.title = element_text(size = 50))) +
    plot_layout(nrow = 2, heights = c(0.55, 0.45))
  
  pdir <- file.path(datadir, "combined_data/finalplots", iso)
  dir.create(pdir, F, T)
  
  ggsave(file.path(pdir, paste0(iso,"_", var, ".png")), pp, device = "png",
         width = 18, height = 24, units = "in")
}

####################################################################################################
iso <- "MDG"
datadir <- "C:/Users/anibi/Documents/ciat/cmip6"
ff <- list.files(datadir, pattern = glob2rx(paste(iso,"tif",sep="*")), 
                 full.names = T, recursive = T)

# preparing in africalab server
pdir <- "/cluster01/workspace/common/climate/worldclim"
cur <- geodata::worldclim_global("bio", "2.5", path = pdir)
cur <- subset(cur, grep("bio_1$|bio_12$", names(cur)))

vg <- geodata::gadm(iso, level=0, path = "/cluster01/workspace/common/vector")
r <- crop(cur, vg)
r <- mask(r, vg, filename= "/data/MDG_bio_1_12_wc2.1_2.5.tif")



# create a stack of historical and future variables
# mean of 2030
fut1 <- rast(grep("2021-2040", ff, value = TRUE))
fut1 <-  subset(fut1, grep("wc2_1$|wc2_12$", names(fut1)))
fut1 <- tapp(fut1, index = c(1,2), fun = mean, na.rm = TRUE)

# mean of 2050
fut2 <- rast(grep("2041-2060", ff, value = TRUE))
fut2 <-  subset(fut2, grep("wc2_1$|wc2_12$", names(fut2)))
fut2 <- tapp(fut2, index = c(1,2), fun = mean, na.rm = TRUE)

# combine all and rename
cur <- rast(grep("bio_1_12", ff, value = TRUE))
fut <- c(fut1, fut2)
fut <- crop(fut, cur)

rr <- c(cur, fut)
names(rr) <- apply(expand.grid(c("AMT_", "ATR_"), c("historical", "2030", "2050")), 1, paste, collapse="")
writeRaster(rr, file.path(datadir, paste0(iso, "_all_bio1_bio12.tif")), overwrite = TRUE)

# admin boundaries
vg <- getData("GADM", country=iso, level=0, path = datadir)

# subset climate data by roi
rr <- stack(file.path(datadir, paste0(iso, "_all_bio1_bio12.tif")))


for (var in c("AMT", "ATR")){
      makePlotCountry(var, varcol, vg, rr, datadir, iso) 
    }


#############################################################################################
# elevation plot
makeElevationPlot <- function(vg, elev, datadir, iso){

  # boundaries
  xlims <- c(xmin(vg), xmax(vg))
  ylims <- c(ymin(vg), ymax(vg))
  
  # fortify for ggplot
  vfort <- fortify(vg)
  
  # color breaks and limits
  mv <- min(cellStats(elev, 'min', na.rm = T))
  mx <- max(cellStats(elev, 'max', na.rm = T))          
  my_limits <- c(round(mv*0.9,0), round(mx*1.1, 0))
  my_breaks <- round(seq(my_limits[1], my_limits[2],  length.out = 3), 0)
  my_limits <- c(ifelse(my_limits[1] > my_breaks[1], my_breaks[1], my_limits[1]),
                 ifelse(my_limits[2] < my_breaks[3], my_breaks[3], my_limits[2]))
  
  
  emap <- gplot(elev) + geom_tile(aes(fill = value)) +
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.8), size = 1) +
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
  
  pdir <- file.path(datadir, "combined_data/finalplots", iso)
  dir.create(pdir, F, T)
  
  ggsave(file.path(pdir, paste0(iso,"_elevation.png")), emap, device = "png",
         width = 12, height = 12, units = "in")
}

elev <- geodata::elevation_30s("MDG", path = file.path(datadir, iso))
elev <- stack(elev)

makeElevationPlot(vg, elev, datadir, iso)


