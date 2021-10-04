# https://www.chpc.utah.edu/documentation/software/r-language.php#rcmd
# srun --pty --nodes=1 --ntasks-per-node=1 --mem-per-cpu=16G --time=00:120:00 bash
# module load R
# version --R
library(terra)
library(raster)

# processing in FARM
ssp <- "585"
res <- "30s"
var <- "wc2.1_30s_bioc_"
period <- c("2021-2040", "2041-2060")

# iso <- c("NGA", "SEN", "NER", "MLI", "ETH", "KEN")
iso <- c("MLI", "ETH", "KEN")
datadir <- "/group/rhijmansgrp/spatial04/worlclim/cmip6/7_fut"
tdir <- "/share/spatial02/users/anighosh/worldclim"

ff <- list.files(datadir, pattern = var, 
                 full.names = TRUE, recursive = TRUE)
ff <- grep(ff, pattern = ssp, value = TRUE)
ff <- grep(ff, pattern = paste(period, collapse = "|"), value = TRUE)

cropByGeometry <- function(f, iso3, tdir){
  r <- rast(f)
  v <- getData('GADM', country=iso3, level=0, path=tdir)
  v <- vect(v)
  
  odir <- file.path(tdir, "cmip6")
  dir.create(odir, FALSE, TRUE)
  
  ofile <- file.path(odir, paste0(iso3, "_" ,basename(f)))
  if(!file.exists(ofile)){
    r <- crop(r, v)
    r <- mask(r, v, filename = ofile, overwrite=TRUE, gdal=c("COMPRESS=LZW"))
  }
  return(NULL)
}
  
for (iso3 in iso){
  cat("processing", iso3, "\n")
  lapply(ff, cropByGeometry, iso3, tdir)
  cat("completed", iso3, "\n")
}

# status of downscaling
ff <- list.files(datadir, pattern = ".tif", 
                 full.names = TRUE, recursive = TRUE)