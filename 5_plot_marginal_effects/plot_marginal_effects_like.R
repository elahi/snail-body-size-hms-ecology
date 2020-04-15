################################################################################
##' @title Plot marginal effects, Littorina keenae
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

source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
source("R/brms_helper_functions.R")

## My quantile
my_p <- 0

## Truncate data
statDat <- truncate_data(dat = childsDF, era = "past", quant = my_p, filter_data = TRUE)

## Offset tidal height for plotting
statDat <- statDat %>% 
  mutate(tideHTm_offset = ifelse(era == "past" & sp == "LIKE", tideHTm + 0.125, 
                                 ifelse(era == "past" & sp == "LODI", tideHTm + 0.025,
                                        tideHTm)))
## Prepare dataset for stats
statDat <- statDat %>%
  mutate(x1z = ifelse(x1 == 0, -0.5, 0.5),
         x2z = as.numeric(scale(x2)),
         x3z = as.numeric(scale(x3)),
         group_j = as.integer(as.factor(sampleUnit)),
         obs_id = seq(1:n()))

statDat <- statDat %>% 
  droplevels() 

x2_mean <- mean(statDat$density_m2)
x2_sd <- sd(statDat$density_m2)

x3_mean <- mean(statDat$tideHTm)
x3_sd <- sd(statDat$tideHTm)

##### GET MODEL #####

## Littorina
model_path <- "brm_models/like/logsize_x123/"
fileNames <- dir(path = model_path, recursive = FALSE, pattern = ".rds")
nFiles <- length(fileNames)
fileNames
i <- 1  ### CHANGE TO GET MODEL OF CHOICE ###
b_final_filename <- paste(model_path, fileNames[i], sep = "")
b_final <- readRDS(b_final_filename)
fit <- b_final
fit
d <- fit$data

## Check to make sure that statDat and d match
#plot(d$size_log, statDat$size_log)

##### PLOT ERA X DENSITY #####

## Prediction data frame
d_new <- statDat %>% 
  group_by(era) %>% 
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
pi_like_density <- d_new_pi %>% 
  mutate(species = "Littorina keenae", 
         covariate = "Density")

##### PLOT ERA X TIDAL HEIGHT #####

## Prediction data frame
d_new <- statDat %>% 
  group_by(era) %>% 
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
tide_offset <- 0.025
pi_like_tide <- d_new_pi %>% 
  mutate(tideHTm_offset = ifelse(era == "past", tideHTm,
                                 tideHTm + tide_offset), 
         species = "Littorina keenae", 
         covariate = "Tidal height")

##### SAVE FOR PLOTTING #####

d_like <- statDat
