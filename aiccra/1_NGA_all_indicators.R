library(raster)
library(stars)
library(rasterVis)
library(tidyverse)
library(tmap)
library(tmaptools)
library(patchwork)
library(grid)
library(classInt)

datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/NIRSAL/NIRSAL/combined_data"

# vector boundaries
# gadm boundary
iso3 <- "NGA"
vg <- getData('GADM', country=iso3, level=1, path = datadir)
clist <- c("Adamawa","Borno","Yobe")
v <- vg[vg$NAME_1 %in% clist, ]

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
rr <- list.files(file.path(datadir), pattern = "interpolated_", full.names = TRUE)

vars <- c("CDD","P5D","P95","NT35","ndws")
seasons <- c("s1", "s2")
gSeasons <- c(1,2)

makePlotCounty <- function(i, varCol, county, season, v, rr, datadir){
  var <- varCol[i,]
  cat("Processing", county, "for", var$vars, season, "\n")
  
  # process spatial data
  # county <- case_when(county == "Trans-Nzoia" ~ "Trans Nzoia",
  #                      county == "Muranga" ~ "Murang'a",
  #                      TRUE ~ county)
  # county specific 
  cbound <- v[v$NAME_1 == county,]
  # celev <- crop(elev, extent(cbound)*1.1)
  
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
  vfort <- fortify(vg)
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
varCol <- varCol[varCol$vars %in% vars,]
# all plots in for loop
for(county in clist){{
  for (season in "s1"){
    for (i in 1:nrow(varCol)){
      # cat("Processing", i, county, season)
      makePlotCounty(i, varCol, county, season, v, rr, datadir) 
    }
  }
}
}
