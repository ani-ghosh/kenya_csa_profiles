# combine data from the two different set of files
library(raster)
library(terra)
library(rasterVis)
library(fst)
library(tidyverse)
library(tmap)
library(tmaptools)
library(patchwork)
library(grid)
library(fields)
library(classInt)

# read each models
readfile <- function(x){
  models <- c("ipsl_cm5a_mr","miroc_esm_chem","ncc_noresm1_m","past")
  # mp <- paste0(models, collapse = "|")
  model <- models[str_detect(x, models)]
  d <- read_fst(x) %>% mutate(model = model)
  return(d)
}


# fill missing pixels
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return(round(mean(x, na.rm=TRUE), 2))
  } else {
    return(round(x[i], 2))
  }
} 


# make high res raster from tables
# makeCountyRaster <- function(d, v1, r1){
#   coordinates(d) <- ~x+y
#   gridded(d) <- TRUE  
#   rd <- raster(d)
#   rd <- extend(rd, extent(v1)*1.25, value=NA)
#   r11 <- focal(rd, w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
#   
#   # replace missing pixels
#   rd <- cover(rd, r11)
#   rd <- resample(rd, r1)
#   rd <- mask(rd, v1)
#   return(rd)
# }

fill.interpolate <- function(rd, r, modeled = FALSE){
  xy <- data.frame(xyFromCell(rd, 1:ncell(rd)))
  vals <- raster::getValues(rd)
  # add elevation
  p <- raster::aggregate(r, res(rd)/res(r))
  
  # remove NAs
  set.seed(1)
  i <- !is.na(vals)
  xy <- xy[i,]
  vals <- vals[i]
  k <- sample(1:nrow(xy), nrow(xy)*0.2)
  xy <- xy[k,]
  vals <- vals[k]
  # z <- raster::extract(p, xy)
  # xyz <- cbind(xy, z)
  
  #### Thin plate spline model
  tps <- Tps(xy, vals)
  # use model to predict values at all locations
  p <- interpolate(r, tps, ext = extent(v))
  p <- mask(p, r)
  # p <- resample(p, r)
  # 
  # if (modeled){
  #   pr <- p
  # } else {
  #   pr <- cover(rd, p)
  # }
  # pr <- crop(pr, v)
  # pr <- mask(pr, v)
  return(p)
}

makeCountryRaster <- function(d, v, r, modeled){
  coordinates(d) <- ~x+y
  gridded(d) <- TRUE  
  rd <- raster(d)
  # rd <- mask(rd, v)
  rd <- extend(rd, extent(v), value=NA)
  # r11 <- focal(rd, w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
  pr <- fill.interpolate(rd, r, modeled)
  
  if (var %in% c("CDD", "NT35")){
    pr <- round(pr)
  }
  return(pr)
}

# theme for ggplot
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

# 3 panel map for each time perios
makeAbsoluteMap <- function(gs, var, vfort, cboundfort, xlims, ylims, ...){
  
  # making sure order is correct
  gs1 <- raster::subset(gs, grep("1985", names(gs)))
  gs2 <- raster::subset(gs, grep("2021", names(gs)))
  gs3 <- raster::subset(gs, grep("2041", names(gs)))
  gs <- raster::stack(gs1, gs2, gs3)
  names(gs) <- c("b1", "b2", "b3")
  # color breaks and limits
  mn <- min(cellStats(gs, 'min', na.rm = T))
  mx <- max(cellStats(gs, 'max', na.rm = T))          
  my_limits <- c(round(mn,0), round(mx, 0))
  
  # cint <- classIntervals(values(gs), 10, style = "quantile")
  # my_breaks <- round(cint$brks, 0)
  # nb <- length(my_breaks)
  # my_labels <- my_breaks[c(1, round(nb/2, 0), nb)]
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
  
  absmap <- gplot(gs) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable, labeller = labeller(variable = 
                                                 c("b1" = "1985-2015",
                                                   "b2" = "2021-2040",
                                                   "b3" = "2041-2060"))) +
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.8), size = 1) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.1), size = 2) +
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
makeChangeMap <- function(gs, var, vfort, cboundfort, xlims, ylims, ...){
  gs1 <- raster::subset(gs, grep("1985", names(gs)))
  gs2 <- raster::subset(gs, grep("2021", names(gs)))
  gs3 <- raster::subset(gs, grep("2041", names(gs)))
  
  # compute changes
  gs21 <- gs2 - gs1
  gs31 <- gs3 - gs1
  gs <- stack(gs21, gs31)
  names(gs) <- c("b1", "b2")
  
  # color breaks and limits
  my_limits_MM <- c(min(cellStats(gs, 'min', na.rm = T)), max(cellStats(gs, 'max', na.rm = T)))
  # cint <- classIntervals(values(gs), 2, style = "quantile")
  # my_breaks_MM <- round(cint$brks, 0)
  my_breaks_MM <- round(seq(my_limits_MM[1], my_limits_MM[2],  length.out= 3), 0)
  my_limits_MM <- c(ifelse(my_limits_MM[1] > my_breaks_MM[1], my_breaks_MM[1], my_limits_MM[1]),
                    ifelse(my_limits_MM[2] < my_breaks_MM[3], my_breaks_MM[3], my_limits_MM[2]))
  
  # main plot
  chgmap <- gplot(gs) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable, labeller = labeller(variable = 
                                                 c("b1" = "Change 2021-2040",
                                                   "b2" = "Change 2041-2060"))) +
    # if want to include Delta "\u0394 2041-2060"
    geom_path(data = vfort, aes(x = long, y = lat, group = group), 
              color = gray(0.8), size = 1) +
    geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
              color = gray(0.1), size = 2) +
    coord_sf(xlim = xlims, ylim = ylims) +
    scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
    scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
    thememap() +
    labs(fill = paste0("Change in ", var$labs), 
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


#####################################################################################################
# prepare files to make raster and plots

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

# all county files
ss <- list.files(datadir, pattern = "_special.fst$", full.names = T, recursive = T)
ff <- list.files(datadir, pattern = "_corrected.fst$", full.names = T, recursive = T)

countylist <- unique(sapply(strsplit(basename(ff), "_"), "[[", 1))
periods <- c("1985_2015", "2021_2045", "2041_2065")

# model -> IEBC
# "Trans-Nzoia" -> "Trans Nzoia"
# "Muranga" -> "Murang'a"
# s <- grep(county, ss, value = TRUE)
# f <- grep(county, ff, value = TRUE)

# read model output and process for all counties together
ofile <- file.path(datadir, "combined_data", "kenya_all_data_combined.fst")
if(!file.exists(ofile)){
  ds <- lapply(ss, readfile)
  ds <- bind_rows(ds)
  
  df <- lapply(ff, readfile)
  df <- bind_rows(df)
  
  # merge
  dfs <- merge(df, ds, by = c("year", "season", "id", "model"))
  dfs <- dfs %>%  mutate(era = case_when(
    year <= 2015 ~ "1985-2015",
    year > 2015 & year <= 2041 ~ "2021-2040",
    year > 2040 & year <= 2065 ~ "2041-2060"))
  
  write_fst(dfs, ofile)
} else {
  dfs <- read_fst(ofile)
}

# aggregate observations and save as high resolution raster

# except gSeason/SLGP/LGP
dfs1 <- dfs %>% 
  dplyr::select(-c(year, ISO3, Country, county, model, gSeason, SLGP, LGP)) %>%
  group_by(id, x, y, era, season) %>% 
  summarise_all(mean, na.rm = TRUE) %>% mutate_at(.vars =  vars(CDD, NT35), .funs = function(x){round(x, 0)}) %>% 
  ungroup() 

write_fst(dfs1, file.path(datadir, "combined_data", "kenya_woGseaon_data_combined.fst"))

for (var in c("CDD","P5D","P95","NT35","ndws","ATR","AMT")){
  for (season in c("s1", "s2")){
    for (era in c("1985-2015","2021-2040","2041-2060")){
      cat("processing ", var, era, season, "\n")
      d <- dfs1[dfs1$season == season & dfs1$era == era, c("x", "y", var)]
      # d <- dd[dd$season == season & dd$era == era, !colnames(dd) %in% c("id", "era", "season")]
      pr <- makeCountryRaster(d, v, elev)
      names(pr) <- paste(var, era, season, sep="_")
      writeRaster(pr, file.path(datadir, "combined_data/interim", paste0("interpolated_",names(pr), ".tif")), overwrite = TRUE)
    }
  }
}

# gSeason/SLGP/LGP
# why does it take so much time?
dfs2 <- dfs %>% 
  dplyr::select(id, x, y, era, gSeason, SLGP, LGP) %>% 
  group_by(id, era, x, y, gSeason) %>% 
  summarise_all(.f = function(x){round(mean(x, na.rm = TRUE), 0)}) %>% 
  arrange(gSeason) %>% 
  tidyr::nest(data = c('x', 'y','SLGP', 'LGP')) %>% 
  drop_na() %>% 
  filter(gSeason < 3) %>% 
  tidyr::unnest() %>% 
  ungroup()

write_fst(dfs2, file.path(datadir, "combined_data", "kenya_wGseaon_data_combined.fst"))

for (var in c("SLGP", "LGP")){
  for (gSeason in c(1, 2)){
    for (era in c("1985-2015","2021-2040","2041-2060")){
      cat("processing ", var, era, gSeason, "\n")
      d <- dfs2[dfs2$gSeason == gSeason & dfs2$era == era, c("x", "y", var)]
      # d <- dd[dd$season == season & dd$era == era, !colnames(dd) %in% c("id", "era", "season")]
      pr <- makeCountryRaster(d, v, r, modeled = TRUE)
      pr[pr > 365] <- 365
      names(pr) <- paste(var, era, paste0("s",gSeason), sep="_")
      writeRaster(pr, file.path(datadir, "combined_data/interim", paste0("interpolated_",names(pr), ".tif")), overwrite = TRUE)
    }
  }
}


#########################################################################################################
# make maps