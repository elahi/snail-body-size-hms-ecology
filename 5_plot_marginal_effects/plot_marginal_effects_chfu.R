################################################################################
##' @title Plot marginal effects, Tegula funebralis
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-17
################################################################################

##### PACKAGES, DATA #####

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(brms)
library(tidybayes)
library(tidyverse)
library(modelr)

##### GET DATA #####

source("3_analyse_data/02_explore_data_chfu_means.R")

statDat <- statDat %>% 
  droplevels() %>% 
  mutate(species = "Tegula funebralis")

x2_mean <- mean(statDat$density_m2)
x2_sd <- sd(statDat$density_m2)

x3_mean <- mean(statDat$tideHTm)
x3_sd <- sd(statDat$tideHTm)

##### GET MODEL #####

## Tegula
model_path <- "brm_models/chfu/logsize_x123p/"
fileNames <- dir(path = model_path, recursive = FALSE, pattern = ".rds")
nFiles <- length(fileNames)
fileNames
i <- 1  ### CHANGE TO GET MODEL OF CHOICE ###
b_final_filename <- paste(model_path, fileNames[i], sep = "")
b_final <- readRDS(b_final_filename)
fit <- b_final

d <- fit$data

## Check to make sure that statDat and d match
plot(d$size_log, statDat$size_log)

##### PLOT ERA X DENSITY #####

## Prediction data frame
d_new <- statDat %>% 
  group_by(sampleArea, era) %>% 
  data_grid(era, x2z = seq_range(x2z, n = 101), x3z = 0) %>% 
  ungroup()

## Get draws
d_new <- add_fitted_draws(newdata = d_new, model = fit, re_formula = NA)

## Get point intervals
d_new_pi <- d_new %>% 
  point_interval(.width = c(0.8, 0.95))

## Back-transform covariates
d_new_pi <- d_new_pi %>%
  mutate(density_m2 = x2z * x2_sd + x2_mean)

## Save as new object
pi_chfu_density <- d_new_pi %>% 
  mutate(species = "Tegula funebralis", 
         covariate = "Density")

##### PLOT ERA X TIDAL HEIGHT #####

## Prediction data frame
d_new <- statDat %>% 
  group_by(sampleArea, era) %>% 
  data_grid(era, x3z = seq_range(x3z, n = 101), x2z = 0) %>% 
  ungroup()

## Get draws
d_new <- add_fitted_draws(newdata = d_new, model = fit, re_formula = NA)

## Get point intervals
d_new_pi <- d_new %>% 
  point_interval(.width = c(0.8, 0.95))

## Back-transform covariates
d_new_pi <- d_new_pi %>%
  mutate(tideHTm = x3z * x3_sd + x3_mean)

## Save as new object
pi_chfu_tide <- d_new_pi %>% 
  mutate(species = "Tegula funebralis", 
         covariate = "Tidal height")

##### SAVE FOR PLOTTING #####

d_chfu <- statDat
