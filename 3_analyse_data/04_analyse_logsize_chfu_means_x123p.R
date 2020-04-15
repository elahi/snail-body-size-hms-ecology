################################################################################
##' @title Analyze log size, Tegula (Chlorostoma) funebralis (mean body size)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-06
################################################################################

##### PACKAGES, DATA #####
source("3_analyse_data/02_explore_data_chfu_means.R")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(brms)

# Get correlation plot (all <= abs(0.3))
statDat %>% 
  select(era01, density_m2, tideHTm) %>% Mypairs()

# Variance inflation factors (all <= 1.2)
statDat %>% 
  select(era01, density_m2, tideHTm) %>% corvif()

model_path <- "brm_models/chfu/logsize_x123p/"

###### FINAL BRMS MODEL ######

CHAINS <- 4
CORES <- 4
SEED <- 101
ITER <- 2000

b_final <- 
  brm(data = statDat, family = gaussian,
      formula = size_log ~ sampleArea + era*x2z + era*x3z, 
      prior = c(prior(normal(2, 2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(1), class = sigma)),
      control = list(adapt_delta = 0.8), 
      cores = CORES, iter = ITER, chains = CHAINS, seed = SEED)

saveRDS(b_final, file = paste(model_path, "b.rds", sep = ""))

  