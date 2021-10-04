library(terra)
library(raster)
library(viridis)
library(ggplot2)
library(RColorBrewer)
library(Cairo)

source("0_functions.R")
#************************************************************************************
# data preparation in the ciatlabs
datadir <- "/cluster01/workspace/ONECGIAR/Data/climate/worldclim/CMIP6"
iso <- "PAK"
res <- 2.5
  
odir <- file.path(datadir, "output", iso)
dir.create(odir, FALSE, TRUE)

v <- raster::getData(name = 'GADM', country = iso, level = 1, path = odir)
v <- vect(v)
v <- v[v$NAME_1 == "Baluchistan",]

# current bioclim
bc <- geodata::worldclim_global(var, res, datadir)
bc <- subset(bc, grep("bio_1$|bio_12$",names(bc), value = TRUE))
bc <- crop(bc, v)
bc <- mask(bc, v, filename = path.expand(file.path(odir, "current_bio1_12.tif")))

# future climate
models <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")
ssp <- 585
times <- c("2021-2040", "2041-2060", "2061-2080")

# mask and save all bio files
saveISObio <- function(model, time, ssp, var = "bioc", res, v, odir){
  r <- geodata::cmip6_world(model, ssp, time, var, res, path)
  r <- subset(r, grep("_1$|_12$",names(r), value = TRUE))
  r <- crop(r, v)
  ofile <- file.path(odir, paste0(model,"_",time,"_bio1_12.tif")) 
  mask(r, v, filename = ofile, overwrite = TRUE)
}

for (model in models){
  for (time in times){
    tryCatch(saveISObio(model, time, ssp, var = "bioc", res, v, odir),
             error = function(e){return(NULL)})
  }
}

# zip -r ~/output/PAK/baloch.zip PAK
#**************************************************************************************
# save mean for each year and calculate difference and stat
bc <- rast(file.path(odir, "current_bio1_12.tif"))
ff <- list.files(odir, pattern = ".tif", full.names = TRUE)

computeChangeStat <- function(time, ff, bc){
  f <- grep(time, ff, value = TRUE)
  n <- length(f)
  print(n)
  rr <- rast(f)
  # mean annual temperature
  bf1 <- subset(rr, c(1:n*2-1))
  bf1 <- mean(bf1)
  # mean annual rainfall
  bf2 <- subset(rr, c(1:n*2))
  bf2 <- mean(bf2)
  
  # temp difference
  dtemp <- (bf1 - bc[[1]])
  
  # prec diff arithmetic
  dprec <- (bf2 - bc[[2]])
  
  # stack
  dd <- c(dtemp, dprec)
  return(dd)
}

dd <- lapply(times, computeChangeStat, ff, bc)
dd <- rast(dd)

k <- length(times)
years <- c("2030", "2050", "2070")
temp <- c(bc[[1]], subset(dd, c(1:k*2-1)))
# temp <- round(temp, 3)

prec <- c(bc[[2]], subset(dd, c(1:k*2)))
prec <- round(prec)

# get the raster realm
temp <- stack(temp)
names(temp) <- c("current", paste0("change_", years))
prec <- stack(prec)
names(prec) <- c("current", paste0("change_", years))
gadm1 <- raster::getData(name = 'GADM', country = iso, level = 1, path = odir)
gadm1 <- gadm1[gadm1$NAME_1 == "Baluchistan",]

writeRaster(prec, "output/PAK/prec.tif")
writeRaster(temp, "output/PAK/temp.tif")

# first temperature
rng <- round(2 * (maxValue(temp[[1]]) - minValue(temp[[1]]))/10) / 2
mb <- mapbar(temp[[1]], rng)
ylab <- bquote(atop("Annual Mean Temperature",
                    (~degree~C)))

png("output/PAK/current_temperature.png", width = 7, height = 4, units = "in", res = 300)
# svg(file.path(odir,"current_temperature.svg"), width = 6, height = 6)
plotmapbar(mb, ylab, cols <- function(x) rev(colorRampPalette(brewer.pal(9, "RdYlBu"))(x)))
dev.off()

# cairo_ps(file.path(odir,"current_temperature.EPS"), width = 6, height = 6, pointsize = 12, fallback_resolution = 300)
# plotmapbar(mb, ylab, cols <- function(x) rev(colorRampPalette(brewer.pal(9, "RdYlBu"))(x)))
# dev.off()


# plot changes
pols <- list("sp.lines", as(gadm1, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
# colorkey=list(labels=list(cex=1, font=2, col="brown"), height=1, width=1.4, title=expression(m^3/m^3), row=3, column=1, vjust=2)

toplot <- paste0("change_", years)                                                                                                                                                                                                                                                   
tmin <- min(cellStats(temp[[toplot]], "min"))
tmax <- max(cellStats(temp[[toplot]], "max"))
vbrk <- round(seq(tmin, tmax, length.out = 15), 2)

cols <- rev(viridis(n = length(vbrk), option="inferno"))
cols <- cols[seq(1,100,length(vbrk))]

png("output/PAK/temperature_change.png", width = 8, height = 4, units = "in", res = 300)
# svg(file.path(odir,"temperature_change.svg"), width = 8, height = 4)
spplot(temp, zcol = toplot, 
       layout = c(k, 1), as.table = TRUE,
       names.attr = toplot,
       sp.layout = list(pols), col = 'transparent',
       col.regions = cols, 
       at = vbrk,
       colorkey = list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5, labels = vbrk),
       sub = list(expression("Change in temperature "(~degree~C))),
       par.settings = list(axis.line = list(col = 'transparent'), 
                           strip.background = list(col = "transparent"),
                           strip.border = list(col = 'transparent')),
       par.strip.text = list(cex = 1.25),
       # # add padding between plots
       xlim = c(gadm1@bbox["x", "min"] - 0.1,
                gadm1@bbox["x", "max"] + 0.1),
       ylim = c(gadm1@bbox["y", "min"] - 0.1,
                gadm1@bbox["y", "max"] + 0.1))
dev.off()



# precipitation
rng <- round((maxValue(prec[[1]]) - minValue(prec[[1]]) ) / 10, -1)
rng <- round(2*rng, -2)/2
mb <- mapbar(prec[[1]], rng)
ylab1 <- bquote(atop("Total Annual Rainfall",
            "(mm)"))

png("output/PAK/current_prec.png", width = 7, height = 4, units = "in", res = 300)
plotmapbar(mb, ylab1, cols <- function(x) colorRampPalette(brewer.pal(9, "YlGnBu"))(x))
dev.off()


# plot the changes
pols <- list("sp.lines", as(gadm1, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
#cols <- rev(viridis(n = 100, option="viridis"))[1:50]
#cols <- cols[seq(1,50,3)

toplot <- paste0("change_", years)                                                                                                                                                                                                                                                   
pmin <- min(cellStats(prec[[toplot]], "min"))
pmax <- max(cellStats(prec[[toplot]], "max"))
vbrk <- round(seq(pmin, pmax, length.out = 15), 2)

cols1 <- colorRampPalette(brewer.pal(9, "RdBu"))(length(vbrk))[1:sum(vbrk < 0)]
cols2 <- colorRampPalette(brewer.pal(9, "Blues"))(length(vbrk))[1:(sum(vbrk > 0)-1)]
cols <- c(cols1, cols2)

png(file.path("output/PAK","precipitation_change.png"), width = 8, height = 4, units = "in", res = 300)
spplot(prec, zcol = toplot, 
       layout = c(k, 1), as.table = TRUE,
       names.attr = toplot,
       sp.layout = list(pols), col = 'transparent',
       col.regions = cols, 
       at = vbrk,
       colorkey = list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5, labels = vbrk),
       sub = list(expression("Change in precipitation (mm)")),
       par.settings = list(axis.line = list(col = 'transparent'), 
                           strip.background = list(col = "transparent"),
                           strip.border = list(col = 'transparent')),
       par.strip.text = list(cex = 1.25),
       # # add padding between plots
       xlim = c(gadm1@bbox["x", "min"] - 0.1,
                gadm1@bbox["x", "max"] + 0.1),
       ylim = c(gadm1@bbox["y", "min"] - 0.1,
                gadm1@bbox["y", "max"] + 0.1))
dev.off()


#***********************************************************************************************
# plot chart
tdm <- cellStats(temp, "mean")[-1]
tds <- cellStats(temp, "sd")[-1]
td <- data.frame(time = year, mean = tdm, sd = tds)


ggplot(td, aes(time, mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Temperature Change", x = "Year", y = "Change in temperature") +
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )+
  theme_bw() +
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.5,
              fill = "grey70",
              colour=NA
  )

ggsave(file.path("output/PAK", "temp_change_plot.png"), width = 4, height = 2, 
       units = "in", dpi = 200)


#***********************************************************************************************
# plot chart
tpm <- cellStats(prec, "mean")[-1]
tps <- cellStats(prec, "sd")[-1]
tp <- data.frame(time = zoo::as.yearmon(years), mean = tpm, sd = tps)


ggplot(tp, aes(time, mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Precipitation Change", x = "Year", y = "Change in precipitation \n(in mm)") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )+
  theme_bw() +
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.5,
              fill = "grey70",
              colour=NA
  )
ggsave(file.path(odir, "prec_change_plot.svg"), width = 4, height = 2, 
       units = "in", dpi = 200, device = "svg")


# min and max
dd <- rbind(cellStats(temp, "min"), cellStats(temp, "max"), cellStats(prec, "min"), cellStats(prec, "max"))
row.names(dd) <- c("temp_min", "temp_max", "prec_min", "prec_max")
colnames(dd) <- c("current", "change_2030", "change_2050", "change_2070", "change_2080")
