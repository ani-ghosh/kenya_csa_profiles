.wcurl <- "https://biogeo.ucdavis.edu/cmip6/"

cmip6_world <- function(model, ssp, time, var, res, path, v) {
  
  res <- as.character(res)
  stopifnot(res %in% c("2.5", "5", "10", "0.5"))
  stopifnot(var %in% c("tmin", "tmax", "prec", "bio", "bioc"))
  ssp <- as.character(ssp)
  stopifnot(ssp %in% c("126", "245", "370", "585"))
  # stopifnot(model %in% c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"))
  stopifnot(model %in% c("ACCESS-CM2","BCC-CSM2-MR","CIESM","CNRM-CM6-1","CNRM-ESM2-1","FIO-ESM-2-0","HadGEM3-GC31-LL","INM-CM5-0",
                         "MIROC-ES2L","MPI-ESM1-2-LR", "ACCESS-ESM1-5","CanESM5","CMCC-ESM2","CNRM-CM6-1-HR","FGOALS-g3",
                         "GISS-E2-1-G","INM-CM4-8", "IPSL-CM6A-LR","MPI-ESM-1-2-HAM","NESM3"))
  stopifnot(time %in% c("2021-2040", "2041-2060", "2061-2080"))
  
  # some combinations do not exist. Catch these here.
  
  if (var == "bio") var <- "bioc"
  stopifnot(dir.exists(path))
  
  fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
  path <- file.path(path, paste0("wc2.1_", fres, "/"))
  dir.create(path, showWarnings=FALSE)
  
  tif <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
  ptif <- file.path(path, tif)
  
  if(!missing(v)){
    ptif <- file.path(path, paste0(v$NAME_1 ,"_", tif))
  }
  
  if (!file.exists(ptif)) {
    url <- file.path(.wcurl, fres, model, paste0("ssp", ssp), tif)
    if(!missing(v)){
      u1 <- file.path("/vsicurl", url)
      r <- terra::rast(u1)
      r <- crop(r, v, snap = "out", filename = ptif)
    } else {
      ok <- try(utils::download.file(url, ptif, mode="wb"), silent=TRUE)
      if (class(ok) == "try-error") {stop("download failed")}
      if (!file.exists(ptif)) {stop("download failed")}
    }
  }
  rast(ptif)
}


# download subset for a geographic area
rwa <- geodata::gadm(country='RWA', level=0, path=tempdir())
r1 <- cmip6_world(model="ACCESS-CM2", ssp="585", time="2021-2040", var="tmin", res=10, path=tempdir(), v=rwa)  
# download global
r2 <- cmip6_world(model="ACCESS-CM2", ssp="585", time="2021-2040", var="tmax", res=10, path=tempdir())


library(terra)
v0 <- geodata::gadm(country = "SEN", level = 1, path = "C:/Users/anibi/Documents/ciat/cmip6/icrisat")
v <- v0[v0$NAME_1 == "Kaffrine",]
lapply(vars, getERASummary, ff, period = "annual", years, sdate, edate, v)


v0 <- getData("GADM", country = "NER", level = 1, path = "/cluster01/workspace/common/vector/gadm")
v <- v0[grep("Tillabéry",v0$NAME_1),]
lapply(vars, getERASummary, ff, period = "annual", years, sdate, edate, v)


v0 <- getData("GADM", country = "MLI", level = 1, path = "/cluster01/workspace/common/vector/gadm")
v <- v0[grep("Segou",v0$NAME_1),]
lapply(vars, getERASummary, ff, period = "annual", years, sdate, edate, v)