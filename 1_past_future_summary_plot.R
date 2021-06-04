library(tidyverse)
library(cowplot)
library(fst)
library(ggthemes)
# library(showtext)
library(extrafont)
# font_import()
loadfonts(device = "win")

datadir <- "G:/My Drive/work/ciat/climate_risk_profiles/kenya_counties/data"

ff <- list.files(datadir, pattern = "_special.fst$", full.names = T, recursive = T)

# make line plots for past
county <- "Vihiga"


f <- grep(county, ff, value = TRUE)
f1 <- grep("past", f, value = TRUE) 

d1 <- read.fst(f1)

# summarize data
d1 <- d1 %>% 
  select(-id) %>%
  group_by(year, season) %>%
  summarise_all(c("mean", "sd")) %>%
  mutate(season = case_when(
    season == "s1" ~ "Season 1",
    season == "s2" ~ "Season 2"))

# future barplots
# f2 <- grep("future", f, value = TRUE) 
d2 <- lapply(f, read.fst)
d2 <- bind_rows(d2) 

# summarize data
d2 <- d2 %>% 
  select(-id) %>%
  # mutate(era = ifelse(year < 2040, "2030", "2050")) %>%
  mutate(era = case_when(
    year <= 2015 ~ "hist",
    year > 2015 & year <= 2041 ~ "2030",
    year > 2040 & year <= 2070 ~ "2050")) %>%
  group_by(era, season) %>%
  summarise_all(c("mean", "sd")) %>%
  mutate(season = case_when(
    season == "s1" ~ "Season 1",
    season == "s2" ~ "Season 2"))

d2$era <- factor(d2$era, levels = c('hist', '2030', '2050'))
trange <- round(c(min(d2$AMT_mean) - 2 , max(d2$AMT_mean) + 2))

font <- "Sans"

# past temeprature pattern
plot1 <- ggplot(d1, aes(year, AMT_mean, col = season)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = trange) +
  labs(x = "Year", y = expression("Temperature "(~degree~C))) +
  theme_classic() +
  theme(text = element_text(size = 20, family = font),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    legend.position = c(.01, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_ribbon(aes(ymax = AMT_mean + AMT_sd, ymin = AMT_mean - AMT_sd, fill = season), alpha = 0.3, colour=NA) +
  scale_fill_manual(values = c("#b30000", "#fd8d3c"), aesthetics = c("color", "fill")) + 
  # scale_x_continuous(labels = sort(unique(d1$year)), breaks = sort(unique(d1$year)))
  scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))

  
plot2 <- ggplot(d2, aes(x=as.factor(season), y=AMT_mean, fill=era)) +
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim = trange) +
  geom_errorbar(aes(ymax = AMT_mean + AMT_sd, ymin = AMT_mean - AMT_sd), width=.2, position=position_dodge(.9)) +
  labs(x = "Year") +
  theme_classic() +
  scale_fill_manual(values = c("#fe9929", "#cc4c02", "#a50f15"), aesthetics = c("color", "fill")) +
  theme(text = element_text(size = 16, family = font),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction="horizontal")

titletext <- ggdraw() + draw_label("Annual Mean Temperature Trends", fontface='bold', size = 20)
pp1 <- plot_grid(plot1, plot2, rel_widths = c(2, 1.2), align = "h")
pp1 <- plot_grid(titletext, pp1, ncol = 1, rel_heights=c(0.1, 1))

save_plot(file.path(datadir, paste0(county, "_temperature_plot.png")), pp1, base_height = 5, base_width = 12)


######################################################################################################
prange <- round(c(min(d2$ATR_mean-d2$ATR_sd) - 200 , max(d2$ATR_mean+d2$ATR_sd) + 200))

# past temeprature pattern
plot3 <- ggplot(d1, aes(year, ATR_mean, col = season)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = prange) +
  labs(x = "Year", y = "Rainfall (mm)") +
  theme_classic() +
  theme(text = element_text(size = 20, family = font),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_ribbon(aes(ymax = ATR_mean + ATR_sd, ymin = ATR_mean - ATR_sd, fill = season), alpha = 0.3, colour=NA) +
  scale_fill_manual(values = c("#a8ddb5", "#2b8cbe"), aesthetics = c("color", "fill")) + 
  # scale_x_continuous(labels = sort(unique(d1$year)), breaks = sort(unique(d1$year)))
  scale_x_continuous(breaks = round(seq(min(d1$year), max(d1$year), by = 5), 1))


plot4 <- ggplot(d2, aes(x=as.factor(season), y=ATR_mean, fill=era)) +
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim = prange) +
  geom_errorbar(aes(ymax = ATR_mean + ATR_sd, ymin = ATR_mean - ATR_sd), width=.2, position=position_dodge(.9)) +
  labs(x = "Year") +
  theme_classic() +
  scale_fill_manual(values = c("#08519c", "#4292c6", "#9ecae1"), aesthetics = c("color", "fill")) +
  theme(text = element_text(size = 16, family = font),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction="horizontal")

raintitle <- ggdraw() + draw_label("Annual Total Rainfall Trends", fontface='bold', size = 20)
pp2 <- plot_grid(plot3, plot4, rel_widths = c(2, 1.2), align = "h")
pp2 <- plot_grid(raintitle, pp2, ncol = 1, rel_heights=c(0.1, 1))

save_plot(file.path(datadir, paste0(county, "_rainfall_plot.png")), pp2, base_height = 5, base_width = 12)

