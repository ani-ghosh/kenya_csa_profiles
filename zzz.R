datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/kenya_counties/data"

# vector boundaries
# gadm boundary
vg <- getData('GADM', country='KEN', level=1, path = datadir)
# IEBC boundary
ve <- shapefile(file.path(datadir, "ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp"))
clist <- c("Bungoma","Kiambu","Kirinyaga","Kisii","Kitui","Migori",     
           "Murang'a","Nandi","Narok","Nyamira","Samburu","Trans Nzoia","Turkana","Vihiga")
v <- ve[ve$ADM1_EN %in% clist, ]
# reading global elevation because of disputed areas
# r <- stack("G:/My Drive/work/ciat/eia/analysis/input/elevation/wc2.1_30s_elev.tif")
# # crop to Kenya iebc boundary
# r <- crop(r, extent(v)*1.1, filename = file.path(datadir, "kenya_elevation_1km.tif"), overwrite = TRUE)
r <- stack(file.path(datadir, "kenya_elevation_1km.tif"))
r <- crop(r, v)
elev <- mask(r, v)

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

# stack of all raster by variables
rr <- list.files(file.path(datadir, "combined_data/interim"), pattern = "interpolated_", full.names = TRUE)

vars <- c("CDD","P5D","P95","NT35","ndws","ATR","AMT", "SLGP", "LGP")
seasons <- c("s1", "s2")
gSeasons <- c(1,2)

makePlotCounty <- function(i, varCol, county, season, v, elev, rr, datadir){
  var <- varCol[i,]
  cat("Processing", county, "for", var$vars, season, "\n")
  
  # process spatial data
  # county <- case_when(county == "Trans-Nzoia" ~ "Trans Nzoia",
  #                      county == "Muranga" ~ "Murang'a",
  #                      TRUE ~ county)
  # county specific 
  cbound <- v[v$ADM1_EN == county,]
  celev <- crop(elev, extent(cbound)*1.1)
  
  # check files
  cdir <- file.path(datadir, "combined_data/interim", county)
  dir.create(cdir, F, T)
  ofile <- file.path(cdir, paste0(county, "_", var$vars,"_",season, ".tif"))
  # unlink(ofile)
  if(!file.exists(ofile)){
    rv <- grep(paste0("_",var$vars, "_"), rr, value = TRUE)
    rvs <- grep(paste0("_",season), rv, value = TRUE )
    gs <- rast(rvs)
    vc <- vect(cbound)
    crs(gs) <- crs(vc)
    gs <- crop(gs, vc, snap = "in")
    names(gs) <- gsub(".tif","",basename(rvs))
    gs <- mask(gs, vc, filename = ofile, overwrite = TRUE)
  } 
  gs <- stack(ofile)
  
  # boundaries
  xlims <- c(xmin(cbound), xmax(cbound))
  ylims <- c(ymin(cbound), ymax(cbound))
  
  # fortify for ggplot
  vfort <- fortify(ve)
  cboundfort <- fortify(cbound)
  
  # maps
  p1 <- makeAbsoluteMap(gs, var, vfort, cboundfort, xlims, ylims)
  
  p2 <- makeChangeMap(gs, var, vfort, cboundfort, xlims, ylims) 

  if (var$vars %in% c("SLGP", "LGP")){
    fseason <- case_when(
      season == "s1" ~ "gSeason 1",
      season == "s2" ~ "gSeason 2")
  } else{
    fseason <- case_when(
      season == "s1" ~ "Long Rain",
      season == "s2" ~ "Short Rain")
  }
  
  # merge 
  pp <- p1 + theme(plot.margin = unit(c(0.5,0,1,0), "in")) +
    p2 + 
    plot_annotation(title = paste(county, var$vars, "for", fseason),
                                  caption = 'Alliance of Bioversity and CIAT',
                                  theme = theme(plot.title = element_text(size = 50))) +
    plot_layout(nrow = 2, heights = c(0.55, 0.45))
  
  pdir <- file.path(datadir, "combined_data/finalplots", county)
  dir.create(pdir, F, T)
  
  ggsave(file.path(pdir, paste0(county,"_", var$vars, "_", fseason, ".png")), pp, device = "png",
         width = 18, height = 24, units = "in")
  
}

# varCol <- varCol[varCol$vars %in% c("LGP", "SLGP"),]
# all plots in for loop
for(county in clist){{
  for (season in seasons){
    for (i in 1:nrow(varCol)){
      # cat("Processing", i, county, season)
      makePlotCounty(i, varCol, county, season, v, elev, rr, datadir) 
    }
  }
 }
}



###############################################################################################################
# elevation plots
datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/kenya_counties/data"
# vector boundaries
# gadm boundary
vg <- getData('GADM', country='KEN', level=1, path = datadir)
# IEBC boundary
ve <- shapefile(file.path(datadir, "ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp"))
clist <- c("Bungoma","Kiambu","Kirinyaga","Kisii","Kitui","Migori",     
           "Murang'a","Nandi","Narok","Nyamira","Samburu","Trans Nzoia","Turkana","Vihiga")
v <- ve[ve$ADM1_EN %in% clist, ]

# elev
r <- stack(file.path(datadir, "kenya_elevation_1km.tif"))
r <- crop(r, v)
elev <- mask(r, v)

makeElevationPlot <- function(county, v, elev, datadir){
  # county boundary
  cat("Processing", county, "\n")
  cbound <- v[v$ADM1_EN == county,]
  celev <- crop(elev, cbound)
  celev <- mask(celev, cbound)
  
  # boundaries
  xlims <- c(xmin(cbound), xmax(cbound))
  ylims <- c(ymin(cbound), ymax(cbound))
  
  # fortify for ggplot
  vfort <- fortify(ve)
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

for(county in clist){
  makeElevationPlot(county, v, elev, datadir)
}
