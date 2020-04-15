################################################################################
##' @title Exploratory data analysis, Tegula (Chlorostoma) funebralis
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-08-31
################################################################################

##' x1 = era
##' x2 = density
##' x3 = tide height

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(tidyverse)

##### SUBSET DATA #####

## Summarized size data (mean size)
statDat <- wara_means %>% 
  droplevels() %>% 
  mutate(era01 = ifelse(era == "past", 0, 1))
glimpse(statDat)

##' The problem with these calculated mean sizes is that I did not remove the smallest snails from the present data
##' So in the following lines of code I will recalculate the mean sizes and densities after removing the smallest individuals

## My quantile for size threshold
my_quantile <- 0.0

## Plot raw data with size threshold
waraDF_sub <- truncate_data(waraDF, era = "past", quant = my_quantile, filter_data = FALSE)
waraDF_sub %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  facet_grid(era ~ species) + 
  geom_vline(aes(xintercept = size_threshold), color = "red")

## Choose data
waraDF_sub <- truncate_data(waraDF, era = "past", quant = my_quantile, filter_data = TRUE)
waraDF_sub %>% count(era, site, sampleArea, sampleUnit)

## Only 9 individuals removed; 
wara_means_new <- waraDF_sub %>%
  group_by(species, sp, site, era, year, nest1, nest2, sampleUnit, sampleArea) %>%
  summarise(size_mean = mean(size1mm),
            size_med = median(size1mm),
            size_sd = sd(size1mm),
            size_n = n(),
            size_se = size_sd/sqrt(size_n),
            size_CI = qt(0.975, df = size_n - 1) * size_se,
            tide_mean = mean(tideHTm)) %>%
  ungroup()

# Turban snails sampled in 0.25m2 quadrats in present
waraPres <- wara_means_new %>% filter(sp == "CHFU" & era == "present") %>%
  mutate(area_sampled = 0.25,
         density_factor = 1/area_sampled,
         density_m2 = size_n * density_factor)
 
# Wara past transect
source("1_prepare_data/prep_wara_past_transect.R")
wara_past_transect

# Compare tidal range for Wara in past and present
range(waraPres$tide_mean)
range(wara_past_transect$tideHTm)

# Limit to plus or minus 0.5m on either side 
min_wara_tide <- min(waraPres$tide_mean) - 0.5

plot(waraPres$tide_mean, y = waraPres$size_mean, xlim = c(-1, 2))
points(wara_past_transect$tideHTm, y = wara_past_transect$size_mm, col = "red")
abline(v = min_wara_tide)

wara_past_transect <- wara_past_transect %>%
  mutate(sampleUnit = site, 
         sampleArea = site, 
         nest1 = NA, 
         nest2 = NA, 
         size_n = NA)

waraPres2 <- waraPres %>% 
  select(species, sp, site, era, tide_mean, size_mean, density_m2, 
         sampleUnit, sampleArea, nest1, nest2, size_n) %>% 
  rename(tideHTm = tide_mean, 
         size_mm = size_mean)

# Remove very low tides for wara_past_transect
wara_past_transect <- wara_past_transect %>% filter(tideHTm > min_wara_tide)
wara_means_new <- rbind(waraPres2, wara_past_transect)

##### CHECK FOR COVARYING PREDICTORS #####

# Use the newly calculated means and densities
statDat <- wara_means_new %>%
  mutate(size_log = log(size_mm), 
         size1mm = size_mm) %>% 
  mutate(era01 = ifelse(era == "past", 0, 1), 
         x1 = era01, x2 = density_m2, x3 = tideHTm)

# Get correlation plot
names(statDat)
statDat %>% 
  select(era01, density_m2, tideHTm) %>% Mypairs()

# Variance inflation factors
statDat %>% 
  select(era01, density_m2, tideHTm) %>% corvif()

##### Examine overlap between density estimates #####

statDat %>% 
  ggplot(aes(density_m2)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  facet_grid(era ~ species)

statDat %>% 
  ggplot(aes(tideHTm, fill = sampleArea)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_grid(era ~ species)

##### VISUALISE DEPENDENT VARIABLE #####

statDat %>% 
  ggplot(aes(density_m2, tideHTm, color = era, size = size1mm)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")

statDat %>% 
  filter(tideHTm > 0) %>% 
  ggplot(aes(density_m2, size1mm, color = era, size = tideHTm)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  #scale_x_log10() + 
  facet_wrap(~ site)

##### PREP DATA FOR REGRESSION ANALYSIS #####

## To use a prior t distribution for alpha and beta (Gelman et al. 2008)
## Binary inputs are shifted to have a mean of 0 and to differ by 1 in their lower and upper conditions
## Other inputs are shifted to have a mean of 0 and scaled to have a sd of 0.5
## "This scaling puts continuous variables on the same scale as symmetric binary inputs (which, taking on the values of +- 0.5, have sd = 0.5)

total_n <- nrow(statDat)
past_n <- statDat %>% count(x1) %>% filter(x1 == 0) %>% select(n) %>% unlist(use.names = F)
past_prop <- past_n / total_n

## Prepare dataset for stats
statDat <- statDat %>% 
  mutate(x1z = ifelse(x1 == 0, -0.5, 0.5), 
         # x2z = scale_gelman(x2), 
         # x3z = scale_gelman(x3), 
         x2z = as.numeric(scale(x2)), 
         x3z = as.numeric(scale(x3)), 
         group_j = as.integer(as.factor(sampleArea)), 
         obs_id = seq(1:n())) 

n_group_j = length(unique(statDat$group_j))
unique(statDat$x1z)
statDat %>% count(era, group_j)
unique(statDat$sampleArea)

## Check gelman scaling (if used)
statDat %>% select(x1, x1z, x2z, x3z) %>% 
  summarise_if(is.numeric, mean) %>% t() %>% round(., 3)
statDat %>% select(x1, x1z, x2z, x3z) %>% 
  summarise_if(is.numeric, sd) %>% t() %>% round(., 3)

## Check histos
statDat %>% 
  ggplot(aes(size_log, fill = era)) + 
  geom_density(alpha = 0.5) 

statDat %>% 
  ggplot(aes(sampleArea, size_log, color = era)) + 
  geom_boxplot()

statDat %>% count(era, sampleArea)
