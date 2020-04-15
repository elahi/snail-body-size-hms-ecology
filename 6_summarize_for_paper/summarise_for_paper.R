################################################################################
##' @title Summarise for paper
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-08
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
library(tidyverse)

##### SPATIAL REPLICATION #####

dat %>% 
  distinct(era, species, sampleArea, sampleUnit) %>% 
  count(species)

dat %>% 
  distinct(era, species, sampleArea, sampleUnit) %>% 
  count(species, era)

##### CHECK FOR COVARYING PREDICTORS #####

## My data
statDat <- waraDF %>% droplevels()
statDat <- childsDF %>% droplevels()
statDat <- hexDF %>% droplevels()

statDat <- wara_means %>% 
  droplevels() %>% 
  mutate(era01 = ifelse(era == "past", 0, 1))

# Variance inflation factors
statDat %>% 
  select(era01, density_m2, tideHTm) %>% corvif()

# Get correlation plot
statDat %>% 
  select(era01, density_m2, tideHTm) %>% Mypairs()

## Largest VIF and correlations:
# waraDF: 1.08, 0.27
# childsDF: 1.59, -0.42
# hexDF: 1.13, 0.25
# wara_means: 1.04, 0.18

##### SPATIAL EXTENT OF SITE #####
# Using google maps
# 600 ft by 255 ft
# 183m by 78m
