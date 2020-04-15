################################################################################
##' @title Analyze log size, Lottia digitalis (x123; full model)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-06
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")
source("R/brms_helper_functions.R")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(brms)

statDat <- hexDF %>% 
  mutate(x1z = ifelse(x1 == 0, -0.5, 0.5), 
         x2z = as.numeric(scale(x2)), 
         x3z = as.numeric(scale(x3)), 
         group_j = as.integer(as.factor(sampleUnit)), 
         obs_id = seq(1:n())) 

model_path <- "brm_models/lodi/logsize_x123/"

###### FINAL BRMS MODEL ######

CHAINS <- 4
CORES <- 4
SEED <- 101
ITER <- 2000

b_final <- 
  brm(data = statDat, family = gaussian,
      formula = size_log ~ era*x2z + era*x3z + (1 | sampleUnit), 
      prior = c(prior(normal(2, 2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(1), class = sigma)),
      control = list(adapt_delta = 0.8), 
      cores = CORES, iter = ITER, chains = CHAINS, seed = SEED)

saveRDS(b_final, file = paste(model_path, "b.rds", sep = ""))

###### RUN TRUNCATED MODELS ######

run_brm_truncated(dat = hexDF, p_vector = seq(0, 0.5, by = 0.05), model_path = model_path)
  
  
  