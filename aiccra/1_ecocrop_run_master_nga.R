# Author: Ani Ghosh 
# Date :  May 2020
# Version 0.1
# Licence GPL v3


# run ecocrop
# setup run
library(raster)
library(dismo)

source("suitability/0b_ecocrop_functions.R")
source("suitability/1_ecocrop_multiple.R")
####################################################################################

# for each model/rcp/year run all crops (original FAO and modified)
datadir <- "/cluster01/workspace/CCI/Data/CMIP5"

##############################################################################################################
# option to read both FAO and modified parameters
# eco1 <- readxl::read_excel("suitability/ecocrop_runs.xlsx", sheet = 1)

# modified parameters
eco2 <- readxl::read_excel("suitability/nga_ecocrop_runs.xlsx", sheet = 1)
names(eco2) <-  c("iso3","crop","crop_calendar","suitability","suitability_seasons","Gmin","Gmax","GSL_months", 
                  "Tkill","Tmin","Topmin","Topmax", "Tmax", "Rmin","Ropmin","Ropmax","Rmax","Comments") 
eco2 <- dplyr::filter(eco2, suitability == "EcoCrop")

# if need to run a specific set of crops
# eco <- eco1[eco1$crop %in% "list_of_crops_as_present_in_the_excel",]
##############################################################################################################
# resolution
k <- 2
pres <- c(0.5, 2.5, 5, 10)
res <- pres[2]

if (res==2.5) { 
  res <- '2_5min' 
} else if (res == 0.5) {
  res <- "30s"
} else {
  res <- paste(res, 'min', sep='')
}

##############################################################################################################
# run on worldclim
vars <- c("tmean","tmin","prec")
wc <- lapply(vars, function(var)getData('worldclim', var=var, res=pres[k], path = datadir))
tavg <- wc[[1]]
tmin <- wc[[2]]
prec <- wc[[3]]  

##############################################################################################################
# output directory
outdir <- "/cluster01/workspace/CCI/Data/climateriskprofiles/results/suitability"

# iso <- "PAK"
eco <- eco2

# country list
# isol <- c("BDI", "GNB" ,"GIN", "HTI", "NPL", "NER", "PAK", "SOM", "TZA")
# isol <- c("BDI", "GNB" ,"GIN", "HTI", "NPL", "NER", "SOM", "TZA")
isol <- "NGA"

for (iso in isol){
  ecop <- eco[!is.na(eco$iso3),]
  ecop <- ecop[grep(iso, ecop$iso3), ]
  ecop$iso3 <- NULL
  ecop$crop <- gsub("[\r\n]", "", ecop$crop)  
  
  cores <- nrow(ecop)
  parallel::mclapply(1:nrow(ecop), runEcocropSingle, ecop, tmin, tavg, prec, 
                     rainfed = FALSE, "worldclim", outdir, res, iso,
                     mc.preschedule = FALSE, mc.cores = cores) 
}


for (iso in isol){
  ecop <- eco[!is.na(eco$iso3),]
  ecop <- ecop[grep(iso, ecop$iso3), ]
  ecop$iso3 <- NULL
  ecop$crop <- gsub("[\r\n]", "", ecop$crop)  
  
  cores <- nrow(ecop)
  parallel::mclapply(1:nrow(ecop), runEcocropSingle, ecop, tmin, tavg, prec, 
                     rainfed = TRUE, "worldclim", outdir, res, iso,
                     mc.preschedule = FALSE, mc.cores = cores) 
}             

################################################################################################################
# future 
# run for future climate
# location of climate data
cmipdir <- file.path(datadir, "cmip5", res)

# list all files
ff <- list.files(cmipdir, pattern = "*.zip", full.names = TRUE)

# we need to run the model for every gcm/rcp/year combination
gcm <- tolower(c("MOHC_HADGEM2_ES", "CESM1_CAM5", "GFDL_CM3", "MPI_ESM_LR", "MIROC_MIROC5"))
rcp <- paste0("rcp", "8_5")
# years <- c(2030, 2050, 2070, 2080)
years <- c(2030, 2050)

# combination of run
runs <- expand.grid(rcp = rcp,  years = years, gcm = gcm, stringsAsFactors = FALSE)             

nc <- nrow(runs)*2

for (iso in isol){
  ecop <- eco[!is.na(eco$iso3),]
  ecop <- ecop[grep(iso, ecop$iso3), ]
  ecop$iso3 <- NULL
  ecop$crop <- gsub("[\r\n]", "", ecop$crop)  
  
  cores <- nc
  parallel::mclapply(1:nrow(runs), runEcocropAll, runs, ff, ecop, outdir, res, rainfed=FALSE, iso=iso,
                     mc.preschedule = FALSE, mc.cores = cores)
  
}

for (iso in isol){
  ecop <- eco[!is.na(eco$iso3),]
  ecop <- ecop[grep(iso, ecop$iso3), ]
  ecop$iso3 <- NULL
  ecop$crop <- gsub("[\r\n]", "", ecop$crop)  
  
  
  cores <- nc
  parallel::mclapply(1:nrow(runs), runEcocropAll, runs, ff, ecop, outdir, res, rainfed=TRUE, iso=iso,
                     mc.preschedule = TRUE, mc.cores = cores)
  
}