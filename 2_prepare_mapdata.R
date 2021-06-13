# combine data from the two different set of files
library(raster)
library(rasterVis)
library(fst)
library(tidyverse)
library(tmap)
library(tmaptools)

fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
} 


readfile <- function(x){
  models <- c("ipsl_cm5a_mr","miroc_esm_chem","ncc_noresm1_m","past")
  # mp <- paste0(models, collapse = "|")
  model <- models[str_detect(x, models)]
  d <- read_fst(x) %>% mutate(model = model)
  return(d)
}

makeCountyRaster <- function(d, v1, r1){
  coordinates(d) <- ~x+y
  gridded(d) <- TRUE  
  rd <- raster(d)
  rd <- extend(rd, extent(v1)*1.25, value=NA)
  r11 <- focal(rd, w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
  
  rd <- cover(rd, r11)
  rd <- resample(rd, r1)
  rd <- mask(rd, v1)
  return(rd)
}

datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/kenya_counties/data"

r <- getData('alt', country='KEN', path = datadir)
v <- getData('GADM', country='KEN', level=1, path = datadir)
ss <- list.files(datadir, pattern = "_special.fst$", full.names = T, recursive = T)
ff <- list.files(datadir, pattern = "_corrected.fst$", full.names = T, recursive = T)

countylist <- unique(sapply(strsplit(basename(ff), "_"), "[[", 1))
periods <- c("1985_2015", "2021_2045", "2041_2065")


# county functions
cat("Prepping", county, "\n")

# process spatial data
cbound <- v[v$NAME_1 == county,]
celev <- crop(r, cbound)

shp_sf <- cbound %>% sf::st_as_sf()
country <- v %>% sf::st_as_sf()
xlims <- sf::st_bbox(shp_sf)[c(1, 3)]
ylims <- sf::st_bbox(shp_sf)[c(2, 4)]

vfort <- fortify(v)
cboundfort <- fortify(cbound)

combineInd <- function(county){}
s <- grep(county, ss, value = TRUE)
f <- grep(county, ff, value = TRUE)

ds <- lapply(s, readfile)
ds <- bind_rows(ds)

df <- lapply(f, readfile)
df <- bind_rows(df)

# merge
dfs <- merge(df, ds, by = c("year", "season", "id", "model"))
dfs <- dfs %>%  mutate(era = case_when(
  year <= 2015 ~ "1985-2015",
  year > 2015 & year <= 2041 ~ "2021-2040",
  year > 2040 & year <= 2065 ~ "2041-2060"))

write_fst(dfs, file.path(datadir, "combined_data", paste0(county, "_all_data_combined.fst")))

# aggregate observations and save as high resolution raster

dfa <- dfs %>% 
  dplyr::select(-c(ISO3,Country,county, year, model, gSeason, LGP, SLGP)) %>%
  group_by(id, x, y, era, season) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup() %>%  
  mutate(season = case_when(
    season == "s1" ~ "Long Rain",
    season == "s2" ~ "Short Rain"))
          
gSeason_i <- dfs %>% 
  dplyr::select(c(id, x, y, year, era, gSeason)) %>%
  group_by(id, x, y, year, era) %>% 
  summarise(gSeason = max(gSeason)) %>%
  ungroup() %>% 
  group_by(era, id, x, y) %>%
  summarise(gSeason = round(mean(gSeason, na.rm = TRUE), 0)) %>% ungroup()          


two_index <- dfs %>% 
  dplyr::select(id, x, y, era, gSeason, SLGP, LGP) %>% 
  group_by(id, era, x, y, gSeason) %>% 
  summarise_all(.f = function(x){round(mean(x, na.rm = TRUE), 0)}) %>% 
  arrange(gSeason) %>% 
  tidyr::nest(data = c('x', 'y','SLGP', 'LGP')) %>% 
  drop_na() %>% 
  filter(gSeason < 3) %>% 
  tidyr::unnest() %>% 
  ungroup()

SLGP_dif <- two_index %>% dplyr::select(-LGP)  %>%
  pivot_wider(names_from = era, values_from = SLGP) %>%
  mutate(dif1 = `2021-2040`-`1985-2015`,
         dif2 = `2041-2060`-`1985-2015`)

gs1 <- list()
cols <- c("1985-2015","2021-2040","2041-2060","dif1","dif2")
slgp1 <- SLGP_dif[SLGP_dif$gSeason == 1, ]

for (i in 1:length(cols)){
  gs1[[i]] <- makeCountyRaster(slgp1[,c("x", "y", cols[i])], cbound, celev)
}
gs1 <- stack(gs1)
# names(gs1) <- paste0("SGLP_", cols)

gs11 <- gs1[[1:3]]

limits_two <- two_index %>% group_by(gSeason) %>%
  dplyr::select(SLGP, LGP) %>% summarise_all(.funs = c('min', 'max')) %>% 
  ungroup()
gS <- unique(two_index$gSeason)

my_limits <- c(limits_two$SLGP_min[i], limits_two$SLGP_max[i])
my_breaks <- round(seq(my_limits[1], my_limits[2],  length.out= 3), 0)
my_limits <- c(ifelse(my_limits[1] > my_breaks[1], my_breaks[1], my_limits[1]) ,ifelse(my_limits[2] < my_breaks[3], my_breaks[3], my_limits[2]))



pfill <- glue::glue('SLGP\n(Day of\nthe year)  ')
ptitle <- glue::glue('gSeason = {i}')

absmap <- gplot(gs11) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  geom_path(data = vfot, aes(x = long, y = lat, group = group), 
              color = gray(0.8), size = 0.1) +
  geom_path(data = cboundfort, aes(x = long, y = lat, group = group), 
            color = gray(0.1), size = 0.1) +
  coord_sf(xlim = xlims, ylim = ylims) +
  labs(fill = pfill, 
       title = ptitle,
       x = 'Longitude', y = 'Latitude') +
  scale_fill_viridis_c(limits =  my_limits, breaks = my_breaks, na.value = NA,
                       guide = guide_colourbar(barwidth = 20, label.theme = element_text(angle = 25, size = 35))) +
  scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
  scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
  theme_bw() + 
  theme(line = element_blank(),
    legend.position = 'bottom', text = element_text(size=35), 
        legend.title=element_text(size=35), 
        legend.spacing = unit(5, units = 'cm'),
        legend.spacing.x = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.5)) 


gs12 <- gs1[[4:5]]
dt_MM <- slgp1$dif1

my_limits_MM <- c(min(dt_MM)-1, max(dt_MM)+1)
my_breaks_MM <- round(seq(my_limits_MM[1], my_limits_MM[2],  length.out= 3), 0)
my_limits_MM <- c(ifelse(my_limits_MM[1] > my_breaks_MM[1], my_breaks_MM[1], my_limits_MM[1]) ,ifelse(my_limits_MM[2] < my_breaks_MM[3], my_breaks_MM[3], my_limits_MM[2]))


chgmap <- gplot(gs12) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  geom_path(data = fortify(v), aes(x = long, y = lat, group = group), 
            color = gray(0.8), size = 0.1) +
  geom_path(data = fortify(cbound), aes(x = long, y = lat, group = group), 
            color = gray(0.1), size = 0.1) +
  coord_sf(xlim = xlims, ylim = ylims) +
  labs(fill = pfill, 
       title = ptitle,
       x = 'Longitude', y = 'Latitude') +
  scale_fill_gradient2(limits =  my_limits_MM, 
                       breaks = my_breaks_MM,
                       na.value = NA,
                       low = '#A50026', mid = 'white', high = '#000099', 
                       guide = guide_colourbar(barwidth = 20, label.theme = element_text(angle = 25, size = 35))) +
  scale_y_continuous(breaks = round(ylims, 2), n.breaks = 3) +
  scale_x_continuous(breaks = round(xlims, 2), n.breaks = 3) +
  theme_bw() + 
  theme(line = element_blank(),
        legend.position = 'bottom', text = element_text(size=35), 
        legend.title=element_text(size=35), 
        legend.spacing = unit(5, units = 'cm'),
        legend.spacing.x = unit(1.0, 'cm'), plot.title = element_text(hjust = 0.5)) 
chgmap
