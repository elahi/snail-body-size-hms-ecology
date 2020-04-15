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

## My data
statDat <- waraDF %>% droplevels()

##### SUBSAMPLE FOR WARA #####

# statDat <- waraDF %>% 
#   droplevels() %>% 
#   filter(sampleArea == "Wara.B" |
#                                era == "past" & sampleArea == "Wara.D")
# 
# set.seed(141)
# pres_d <- waraDF %>% filter(sampleArea == "Wara.D" & era == "present") %>%
#   sample_n(size = 1444)
# 
# statDat <- rbind(statDat, pres_d)

##### CHECK FOR COVARYING PREDICTORS #####
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

## My quantile for size threshold
my_quantile <- 0.0

## Plot raw data with size threshold
statDat <- truncate_data(statDat, era = "past", quant = my_quantile, filter_data = FALSE)
statDat %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  facet_grid(era ~ species) + 
  geom_vline(aes(xintercept = size_threshold), color = "red")

## Choose data
statDat <- truncate_data(statDat, era = "past", quant = my_quantile, filter_data = TRUE)
statDat %>% count(era, site, sampleArea, sampleUnit)

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
         group_j = as.integer(as.factor(sampleUnit)), 
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

