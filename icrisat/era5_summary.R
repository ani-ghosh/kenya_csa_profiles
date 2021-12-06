# first get ERA5 data summary for the season
library(raster)
library(lubridate)

summaryFun <- function(year, f, v, sdate, edate, stat){
  f1 <- grep(year, f, value = TRUE)
  r <- lapply(f1, stack) # regular stack not keeping the name
  r <- stack(r)
  dates <- ymd(gsub("X", "" ,names(r)))
  sdate1 <- ymd(paste0(year, "-", sdate))
  edate1 <- ymd(paste0(year, "-", edate))
  k <- which(dates >= sdate1 & dates <= edate1) 
  r1 <- subset(r, k)
  r1 <- crop(r1, v)
  r1 <- calc(r1, fun = stat)
  d <- extract(r1, v, fun = stat)
  vals <- data.frame(year = year, variable = var, vals = round(d,2))
  return(vals)
}

getERASummary <- function(var, ff, period = "annual", years, sdate, edate, v){
  f <- grep(var, ff, value = TRUE)
  if (grepl("precipitation", tolower(var))){
    stat <- sum
  } else{
    stat <- mean
  }
  
  if (period == "annual"){
    # vals <- lapply(years, summaryFun)
    vals <- parallel::mclapply(years, summaryFun, f, v, sdate, edate, stat, 
                               mc.preschedule = FALSE, mc.cores = 10)
    vals <- data.table::rbindlist(vals, fill = TRUE)
    write.csv(vals, paste0("output/cci_plots/", v$NAME_1, "_", var,".csv"), row.names = FALSE)
  }
}

summaryFun <- function(v, r, stat){
  r1 <- crop(r, v)
  r1 <- calc(r1, fun = stat)
  d <- cellStats(r1, mean)
  vals <- data.frame(adm = v$NAME_1, vals = round(d,2))
  return(vals)
}

yearlyFun <- function(year, f, vv, stat){
  cat("processing", year, "\n")
  f1 <- grep(year, f, value = TRUE)
  r <- stack(f1)
  dd <- lapply(vv, summaryFun, r, stat)
  dd <- data.table::rbindlist(dd, fill = TRUE)
  dd$year <- year
  return(dd)
}

getERAdata <- function(var, ff, years, vv){
  f <- grep(var, ff, value = TRUE)
  if (grepl("precipitation", tolower(var))){
    stat <- sum
  } else{
    stat <- mean
  }
  vals <- parallel::mclapply(years, yearlyFun, f, vv, stat, 
                               mc.preschedule = FALSE, mc.cores = 15)
  vals <- data.table::rbindlist(vals, fill = TRUE)
  write.csv(vals, paste0("~/output/cci_plots/", var,".csv"), row.names = FALSE)
}

datadir <- "/cluster01/workspace/common/climate/era5/sis-agromet/nc"
ff <- list.files(datadir, pattern = ".nc", recursive = TRUE, full.names = TRUE)
vars <- c("Temperature-Air-2m-Mean", "precipitation_flux")
years <- 1985:2015

v0 <- getData("GADM", country = "SEN", level = 1, path = "/cluster01/workspace/common/vector/gadm")
v1 <- v0[v0$NAME_1 == "Kaffrine",]

v0 <- getData("GADM", country = "NER", level = 1, path = "/cluster01/workspace/common/vector/gadm")
v2 <- v0[grep("Tillabéry",v0$NAME_1),]

v0 <- getData("GADM", country = "MLI", level = 1, path = "/cluster01/workspace/common/vector/gadm")
v3 <- v0[grep("Ségou",v0$NAME_1),]

vv <- list(v1, v2, v3)

v0 <- getData("GADM", country = "NGA", level = 1, path = "/cluster01/workspace/common/vector/gadm")
clist <- c("Adamawa","Borno","Yobe")
vv <- lapply(clist, function(x, v0){v0[grep(x,v0$NAME_1),]}, v0)


lapply(vars, getERAdata, ff, years, vv)

##############################################################################################################
# chirps/chirts data
library(chirps)
library(terra)

getCHC <- function(year, v, datadir){
  dates <- c(paste0(year, "-01-01"), paste0(year, "-12-31"))
  r1 <- get_chirps(v, dates, as.raster = TRUE)
  r1 <- mask(r1, v)
  writeRaster(r1, filename = file.path(datadir, "historical", paste0(v$NAME_1,"_",year,"_chirps.tif")), overwrite = TRUE)
  cat("finished chirps for", v$NAME_1, year, "\n")
  r2 <- get_chirts(v, dates, as.raster = TRUE)
  r2 <- mask(r2, v)
  writeRaster(r2, filename = file.path(datadir, "historical", paste0(v$NAME_1,"_",year,"_chirts.tif")), overwrite = TRUE)
  cat("finished chirts for", v$NAME_1, year, "\n")
}

datadir <- "C:/Users/anibi/Documents/ciat/cmip6/icrisat"

v0 <- geodata::gadm(country = "SEN", level = 1, path = datadir)
v1 <- v0[v0$NAME_1 == "Kaffrine",]

v0 <- geodata::gadm(country = "NER", level = 1, path = datadir)
v2 <- v0[grep("Tillabéry",v0$NAME_1),]

v0 <- geodata::gadm(country = "MLI", level = 1, path = datadir)
v3 <- v0[grep("Ségou",v0$NAME_1),]

years <- 1985:2015

vv <- list(v1,v2,v3)
for (i in (1:length(vv))){
  v <- vv[[i]]
  cat("processing", v$NAME_1, "\n")
  lapply(years, getCHC, v, datadir)
}


# summary
x <- global(r1, "mean", na.rm=TRUE)
sum(x$mean)