################################################################################
##' @title Summarise tidal data to get cumulative air exposure
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-11-30
##' @log 
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

source("5_plot_air_exposure/air_exposure_functions.R")

# 91 years of data, hourly intervals
load("output/monterey_tides_1930_2020.RData")
head(dat)

# Load size data (only to get species codes)
source("3_analyse_data/01_sbs_bayes_data.R")
size_dat <- dat %>% 
  mutate(species = as.character(species), 
         species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

##### FINE SCALE DATA #####

##' Load fine scale data for 4 sampling years
load("output/monterey_tides_fine_scale.RData")
##' Load the missing set
load("output/monterey_tides_2014.RData")

dat_fine <- rbind(dat_fine, dat0)

dat <- dat_fine %>% 
  mutate(year = year(DateTime), 
         day = day(DateTime),
         month = month(DateTime), 
         intv = as.numeric(DateTime - lag(DateTime))) %>% 
  select(-Station)

# Fix wacky intervals
dat <- dat %>%
  mutate(intv = ifelse(intv == 10, intv, NA))

##' Identify whether exposed or submerged for each desired tidal height
##' using 'do'

new_df <- data.frame(tidal_height = seq(0, 2, by = 0.1))

dat2 <- new_df %>% group_by(tidal_height) %>% 
  do(classify_air_water(dat, .)) %>% ungroup()

dat2
unique(dat2$intv)

# Coldest == Dec, Jan, Feb
# Warmest == July, Aug, Sep

##' Summarise
##' Daily minutes exposed to air
summary(dat2)

daily_min_df <- dat2 %>% filter(air_water == "air") %>% 
  group_by(tidal_height, year, month, day) %>% 
  summarise(sum_intv = sum(intv, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(season = ifelse(month == 12 | month < 3, 
                         "winter", 
                         ifelse(month > 6 & month < 10, 
                                "summer", "spring_fall")))

daily_min_df %>% group_by(season) %>% tally()

##' Get summary stats by month
monthly_min_df <- daily_min_df %>% 
  group_by(tidal_height, year, month) %>% 
  summarise(mean = mean(sum_intv), 
            sd = sd(sum_intv), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

monthly_min_df %>% 
  filter(tidal_height == 0 | tidal_height == 0.5 |
           tidal_height == 1 | tidal_height == 1.5) %>% 
  ggplot(aes(month, mean, color = as.factor(year))) + 
  geom_line() + 
  facet_wrap(~ tidal_height, scales = "free_y") + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.1, alpha = 0.6)

##' Get summary stats by year
year_min_df <- daily_min_df %>% 
  filter(season != "spring_fall") %>% 
  group_by(tidal_height, year, season) %>% 
  summarise(mean = mean(sum_intv), 
            sd = sd(sum_intv), 
            n = n(), 
            se = sd/sqrt(n), 
            CI = qt(0.975, df = n - 1) * se) %>% 
  ungroup()

year_min_df %>% #filter(tidal_height < 0.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)

year_min_df %>% filter(tidal_height < 0.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)
  
  
year_min_df %>% filter(tidal_height > 1.5) %>% 
  ggplot(aes(tidal_height, mean, color = as.factor(year))) + 
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = mean + CI, 
                    ymin = mean - CI), width = 0.01, alpha = 0.6) +
  facet_wrap(~ season)


