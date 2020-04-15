################################################################################
##' @title Plot intertidal temperatures
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-16
##' @log 
################################################################################

#library(cowplot)
source("R/multiplotF.R")
library(grid)

## Tidal heights for individual snails
source("R/choose_size_data.R")
source("R/choose_size_threshold.R")

# load data - approximated sizes
dat <- choose_size_data(method = "uniform")

# Do not remove any data
dat <- choose_size_threshold(x = dat, era = "past", filter_data = F) %>% 
  filter(!is.na(size1mm))

# Distinct tidal heights for LODI and LIKE
tide_rug <- dat %>% 
  filter(species != "Chlorostoma funebralis") %>% 
  distinct(species, sampleArea, tideHTm) %>% 
  arrange(species, sampleArea, tideHTm) %>% print(n = 1000) %>% 
  ungroup() %>% droplevels()
tide_rug
summary(tide_rug)

## Now get tide heights for wara_means
source("3_analyse_data/02_explore_data_chfu_means.R")
dat <- dat %>% 
  mutate(species = as.character(species), 
         species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

tide_rug_chfu <- wara_means_new %>% 
  mutate(tideHTm = round(tideHTm, digits = 2)) %>% 
  distinct(tideHTm, sampleArea) %>% 
  mutate(species = "Tegula funebralis") %>% 
  arrange(tideHTm)
tide_rug_chfu
summary(tide_rug_chfu)

tide_rug_df <- rbind(tide_rug, tide_rug_chfu)
tide_rug_df %>% 
  group_by(species) %>% 
  summarise(min(tideHTm), 
            max(tideHTm))
summary(tide_rug_df)

# Load air exposure
air_df <- read.csv("output/monterey_air_exposure.csv")
air_df
summary(air_df)
air_df <- air_df %>% filter(tidal_height > 0)

# Load temperature logger data (6 weeks of empirical data)
source("2_summarise_data/summarise_intertidal_temps.R")
tempMeans

# Load limpet hindcasts (15 yrs of modeled body temperatures)
source("2_summarise_data/summarise_limpet_temps.R")
monthly_extremes

names(tempMeans)
names(monthly_extremes)

##' Combine these two datasets
# species, mean, CI, tidalHT, metric, 

temp_logger <- tempMeans %>% 
  filter(metric != "daily_cv" & metric != "daily_mean") %>% 
  select(species, position, microhabitat, tidalHT, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(month = NA, 
         year = NA,
         dataset = "Empirical")

# Rename levels of metric
metric <- temp_logger$metric
metric <- gsub("daily_max", "maximum", metric)
metric <- gsub("daily_med", "median", metric)
metric <- gsub("daily_min", "minimum", metric)

temp_logger$metric <- metric

temp_model <- monthly_extremes %>% ungroup() %>% 
  select(species, position, tidalHT, month, year, 
         metric, mean, sd, n, se, CI) %>% 
  mutate(microhabitat = NA, 
         dataset = "Model")

tempDF <- rbind(temp_logger, temp_model)
tempDF

##### 3 PANEL PLOT WITH AIR EXPOSURE ######
theme_set(theme_bw() + 
            theme(panel.grid  = element_blank(), 
                  strip.background = element_blank()))

dataset_description <- c(
  Model = "Model predictions",
  Empirical = "Empirical measurements"
)

intertidal_text_df <- data.frame(x = c(rep(3.5, 3)),
                      y = c(9.25, 14, 19),
                      text1 = c("Min", "Median", "Max"),
                      dataset = rep("Model", 3),
                      metric = c("minimum", "median", "maximum"))

tempDF$dataset

facet_label_text <- tempDF %>% group_by(dataset) %>% 
  summarise(minValue = min(mean), 
            maxValue = max(mean)) %>%  
  mutate(x = 0, 
         text1 = c("A", "B"), 
         dataset = c("Empirical", "Model"), 
         y = c(38.5, 20.5))

facet_label_text

unique(tempDF$species)
tempDF <- tempDF %>% 
  mutate(species = factor(species, 
                          levels = c("Tegula funebralis", "Lottia digitalis", "Littorina keenae")))

unique(tempDF$metric)

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 8, name = 'RdBu')
brewer.pal(n = 8, name = "RdBu")
col_max <- "#B2182B"
col_min <- "#2166AC"
col_mean <- "black"

tide_range_df <- tide_rug_df %>% 
  group_by(species) %>% 
  summarise(min = min(tideHTm), 
            max = max(tideHTm)) %>% 
  mutate(species = factor(species, 
                          levels = c("Tegula funebralis", "Lottia digitalis", "Littorina keenae")))

max_tide <- max(tide_rug_df$tideHTm)
min_tide <- min(tide_rug_df$tideHTm)

## Get tidal heights of loggers
logger_tidalHT <- rawDat2 %>% 
  distinct(species, position, tidalHT) %>% 
  arrange(species, position, tidalHT) %>% 
  mutate(species = factor(species, 
                          levels = c("Tegula funebralis", "Lottia digitalis", "Littorina keenae")))
logger_tidalHT

tide_range_df$species
logger_tidalHT$species

## Air exposure
display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = "Dark2")
pal_species <- brewer.pal(n = 8, name = "Dark2")[1:3]

panelA <- air_df %>%
  ggplot(aes(tidal_height, air_exposure_proportion * 100)) +
  geom_rect(data = tide_range_df, 
            aes(xmin = min, xmax = max, ymin = 0, ymax = 100, fill = species), 
            inherit.aes = FALSE,
            alpha = 0.3) + 
  geom_point(data = logger_tidalHT, 
             aes(x = tidalHT, y = -2, color = species, shape = species, alpha = 1), 
             inherit.aes = FALSE) + 
  geom_line(size = 1) + 
  scale_color_manual(values = pal_species) + 
  scale_fill_manual(values = pal_species) + 
  scale_shape_manual(values = c(1, 2, 0)) + 
  labs(x = "Tidal height (m)", y = "Air exposure (%)") + 
  annotate(geom = "text", x = -0, y = 95, label = "A") + 
  theme(legend.position = "none") 

panelA

## Empirical rock temps

tempDF2 <- tempDF %>% 
  mutate(species = ifelse(species == "Lottia digitalis", 
                          "Lottia", 
                          as.character(species))) %>% 
  mutate(species = (factor(species, 
                           levels = c("Tegula funebralis", 
                                      "Lottia", 
                                      "Littorina keenae"))))


panelB <- tempDF2 %>% 
  filter(dataset == "Empirical") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  guides(shape = 
           guide_legend(title = "Sampling area", title.position = "top", 
                        title.hjust = 0.5, 
                        title.theme = 
                          element_text(size = 8, face = "bold", angle = 0), 
                        keywidth = 0.1,
                        keyheight = 0.1,
                        default.unit="inch", 
                        override.aes = list(fill = "white", color = "black"), 
                        label.theme = 
                          element_text(angle = 0, size = 8, face =  "italic"))) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0, alpha = 0.8) + 
  geom_line(aes(group = interaction(species, metric)), 
            alpha = 1, size = 0.2) + 
  geom_point(alpha = 0.7, size = 2, fill = "white") + 
  geom_point(data = subset(tempDF2, microhabitat == "crevice"), 
             aes(tidalHT, mean, shape = species, color = metric, fill = metric), 
             alpha = 0.6, size = 2, show.legend = FALSE) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  scale_color_manual(values = c(col_max, col_mean, col_min)) + 
  scale_fill_manual(values = c(col_max, col_mean, col_min)) + 
  scale_shape_manual(values = c(21, 24, 22)) + 
  facet_wrap(~ dataset, ncol = 1, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  annotate(geom = "text", x = 8, y = 38.5, label = "B") + 
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0.05, 0.95)) + 
  theme(legend.title = element_blank()) + 
  theme(legend.background = element_blank())
  
panelB

## Predicted limpet (rock) temperature
panelC <- tempDF %>% 
  filter(dataset == "Model") %>% 
  ggplot(aes(tidalHT, mean, shape = species, color = metric)) + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0, alpha = 1) + 
  geom_line(aes(group = interaction(species, metric)), 
            alpha = 1, size = 0.2) + 
  geom_point(alpha = 0.7, size = 2, fill = "white") + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Tidal height (m)") +   
  theme(strip.background = element_blank()) + 
  guides(color = FALSE) + 
  scale_color_manual(values = c(col_max, col_mean, col_min)) + 
  scale_shape_manual(values = c(21, 24, 22)) + 
  theme(legend.position = "none") +
  facet_wrap(~ dataset, ncol = 1, scales = "free_y", 
             labeller = labeller(dataset = dataset_description)) + 
  geom_text(aes(x, y, label = text1, shape = NULL), 
            data = intertidal_text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  annotate(geom = "text", x = 0, y = 20.5, label = "C")

panelC

## Figure S1

## Combine
layout1 <- matrix(c(1, 1, 2, 2, 2, 3, 3, 3), nrow = 8, byrow = TRUE)
pdf("figs/plot_temperature_intertidal_austro.pdf", width = 3.5, height = 7)
multiplot(panelA, panelB, panelC, layout = layout1)
dev.off()	

