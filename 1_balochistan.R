library(raster)
library(terra)
library(rasterVis)
library(tidyverse)
library(tmap)
library(tmaptools)
library(patchwork)
library(grid)
library(fields)
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
  
  my_breaks <- round(seq(my_limits[1], my_limits[2],  length.out = 3), 0)
  my_limits <- c(ifelse(my_limits[1] > my_breaks[1], my_breaks[1], my_limits[1]),
                 ifelse(my_limits[2] < my_breaks[3], my_breaks[3], my_limits[2]))
  
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
              color = gray(0.2), size = 1.25) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.4), size = 1) +
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
              color = gray(0.2), size = 1.25) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.4), size = 1) +
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



datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/PAK/CMIP6/baloch/"

ff <- list.files(datadir, pattern = ".tif$", full.names = T, recursive = T)

# admin boundaries
r <- getData('alt', country='PAK', path=datadir)
v <- getData('GADM', country='PAK', level=1, path=datadir)
cbound <- getData('GADM', country='PAK', level=2, path=datadir)
cbound <- cbound[cbound$NAME_1 == "Baluchistan",]

xlims <- c(xmin(cbound), xmax(cbound))
ylims <- c(ymin(cbound), ymax(cbound))

# fortify for ggplot
vfort <- fortify(ve)
cboundfort <- fortify(cbound)

# create a stack of historical and future variables
cur <- rast(grep("current", ff, value = TRUE))
# mean of 2030
fut1 <- rast(grep("2021-2040", ff, value = TRUE))
fut1 <- tapp(fut1, index = c(1,2), fun = mean, na.rm = TRUE)

# mean of 2050
fut2 <- rast(grep("2041-2060", ff, value = TRUE))
fut2 <- tapp(fut2, index = c(1,2), fun = mean, na.rm = TRUE)

# combine all and rename
rr <- c(cur, fut1, fut2)
names(rr) <- apply(expand.grid(c("AMT_", "ATR_"), c("historical", "2030", "2050")), 1, paste, collapse="")
writeRaster(rr, file.path(datadir, "all_bio1_bio12.tif"))

rr <- stack(file.path(datadir, "all_bio1_bio12.tif"))

# maps
p1 <- makeAbsMapRaster("AMT", rr,  varCol, vfort, cboundfort, xlims, ylims)
# maps
p2 <- makeAbsMapRaster("ATR", rr,  varCol, vfort, cboundfort, xlims, ylims)
# maps

p11 <- makeChangeMapRaster("AMT", rr, varCol, vfort, cboundfort, xlims, ylims)
p21 <- makeChangeMapRaster("ATR", rr, varCol, vfort, cboundfort, xlims, ylims)

# merge 
pp1 <- p1 + theme(plot.margin = unit(c(0.5,0,1,0), "in")) +
  p11 + 
  plot_annotation(title = "Annual Mean Temperature variation under CMIP6 projections",
                  caption = 'Alliance of Bioversity and CIAT',
                  theme = theme(plot.title = element_text(size = 36))) +
  plot_layout(nrow = 2, heights = c(0.55, 0.45))

ggsave(file.path(datadir, "AMT_balochistan.png"), pp1, device = "png",
       width = 20, height = 18, units = "in")


# merge 
pp2 <- p2 + theme(plot.margin = unit(c(0.5,0,1,0), "in")) +
  p21 + 
  plot_annotation(title = "Annual Total Rainfall variation under CMIP6 projections",
                  caption = 'Alliance of Bioversity and CIAT',
                  theme = theme(plot.title = element_text(size = 36))) +
  plot_layout(nrow = 2, heights = c(0.55, 0.45))

ggsave(file.path(datadir, "ATR_balochistan.png"), pp2, device = "png",
       width = 20, height = 18, units = "in")


# summary statistics
dd <- extract(rr, cbound, mean, na.rm = TRUE)
dd <- data.frame(district = cbound$NAME_2, dd)
write.csv(dd, file.path(datadir, "summary_stat_bio1_12.csv"), row.names = FALSE)
