################################################################################
##' @title Extract model summaries, Lottia digitalis (x123; full model)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-09
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(brms)
library(tidybayes)
theme_set(theme_tidybayes())
library(broom)

source("R/brms_helper_functions.R")

##### EXTRACT DATA FROM B_FINAL #####
model_path <- "brm_models/lodi/logsize_x123/"

# Get the most recent file, assumes that the files are ordered chronologically
fileNames <- dir(path = model_path, recursive = FALSE, pattern = ".rds")
nFiles <- length(fileNames)
fileNames

b_final_filename <- paste(model_path, "/", fileNames[length(fileNames)], sep = "")
b_final <- readRDS(b_final_filename)

## Extract model coefficients
fit <- b_final
coefs_df <- extract_coefs(fit = fit, truncate_logic = FALSE, my_quantile = NA)

## Trace plots
stanplot(b_final, type = "trace", facet_args = list(nrow = 4))
ggsave(filename = paste(model_path, "plot_trace/b_trace", ".png", sep = ""), height = 10, width = 7)

## Data, fitted and predicted draws
df <- fit$data %>% as_tibble()
df <- fit_n_predict(dat = df, model = fit)

## Calculate Bayesian p-value for squared discrepancy
bpv_discrepancy <- calculate_bpv(dat = df)

## Plot residuals against fitted values
df %>% 
  sample_n(size = 1) %>% 
  ggplot(aes(.value, .residual)) +
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  labs(x = "Fitted", y = "Residual")
ggsave(filename = paste(model_path, "plot_resids/b_resids", ".png", sep = ""), height = 3.5, width = 5)

## Extract Bayesian R2; sneak in Bayesian p-values
r_sq_df <- bayes_R2(fit) %>% 
  as_tibble() %>% 
  mutate(bpv_discrep = bpv_discrepancy, 
         truncate = FALSE, quantile = NA)
r_sq_df

## Extract rhats
rhats <- rhat(fit)
rhats_df <- tibble(.variable = names(rhats), 
                       rhat = as.numeric(rhats), 
                   truncate = FALSE, quantile = NA)
rhats_df

## Extract neff ratio
n_samples <- 4000
neff_ratios <- neff_ratio(fit)
neff_df <- tibble(.variable = names(neff_ratios), 
                  rhat = as.numeric(neff_ratios), 
                  eff_samples = rhat * n_samples, 
                  truncate = FALSE, quantile = NA)
neff_df

## Extract ICs
waic_b <- waic(fit)
loo_b <- loo(fit)
waic_df <- waic_b$estimates %>% tidy()
loo_df <- loo_b$estimates %>% tidy()
ic_df <- rbind(waic_df, loo_df) %>% 
  mutate(truncate = FALSE, quantile = NA)
ic_df

## Run loop to get the summaries from the truncated data
for(i in 1:(nFiles-1)){
  
  # Get file
  b_final_filename <- paste(model_path, fileNames[i], sep = "")
  b_final <- readRDS(b_final_filename)
  
  # Get quantile
  split1 <- strsplit(fileNames[i], split = "_")[[1]][2]
  split2 <- as.numeric(unlist(strsplit(split1, split = "[.]"))[1])
  my_quantile <- split2 / 100
  
  fit <- b_final
  
  ##### Extract model coefficients #####
  coefs <- extract_coefs(fit = fit, truncate_logic = TRUE, my_quantile = my_quantile)
  coefs_df <- rbind(coefs_df, coefs)
  
  ## Trace plots
  stanplot(fit, type = "trace", facet_args = list(nrow = 4))
  ggsave(filename = paste(model_path, "plot_trace/b_trace_", split2, ".png", sep = ""), 
         height = 10, width = 7)

  ## Data, fitted and predicted draws
  df <- fit$data %>% as_tibble()
  df <- fit_n_predict(dat = df, model = fit)
  
  ## Calculate Bayesian p-value for squared discrepancy
  bpv_discrepancy <- calculate_bpv(dat = df)
  
  ## Plot residuals against fitted values
  df %>% 
    sample_n(size = 1) %>% 
    ggplot(aes(.value, .residual)) +
    geom_point(alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
    labs(x = "Fitted", y = "Residual")
  ggsave(filename = paste(model_path, "plot_resids/b_resids_", split2, ".png", sep = ""), 
         height = 3.5, width = 5)
  
  ##### Extract neff ratio #####
  n_samples <- 4000
  neff_ratios <- neff_ratio(fit)
  neff_df_i <- tibble(.variable = names(neff_ratios), 
                    rhat = as.numeric(neff_ratios), 
                    eff_samples = rhat * n_samples, 
                    truncate = TRUE, quantile = my_quantile)
  neff_df <- rbind(neff_df, neff_df_i)
  
  ##### Extract Bayesian R2 #####
  r_sq_df_i <- bayes_R2(fit) %>% 
    as_tibble() %>% 
    mutate(bpv_discrep = bpv_discrepancy, 
           truncate = TRUE, quantile = my_quantile)
  r_sq_df <- rbind(r_sq_df, r_sq_df_i)
  
  ##### Extract rhats #####
  rhats <- rhat(fit)
  rhats_df_i <- tibble(.variable = names(rhats), 
                     rhat = as.numeric(rhats), 
                     truncate = TRUE, quantile = my_quantile)
  rhats_df <- rbind(rhats_df, rhats_df_i)
  
  ##### Extract ICs #####
  waic_b <- waic(fit)
  loo_b <- loo(fit)
  waic_df <- waic_b$estimates %>% tidy()
  loo_df <- loo_b$estimates %>% tidy()
  ic_df_i <- rbind(waic_df, loo_df) %>% 
    mutate(truncate = TRUE, quantile = my_quantile)
  ic_df <- rbind(ic_df, ic_df_i)
  
}

##### SAVE FILES #####

write.csv(coefs_df, file = paste(model_path, "model_summaries/", "coefs_df.csv", sep = ""))
write.csv(r_sq_df, file = paste(model_path, "model_summaries/", "r_sq_df.csv", sep = ""))
write.csv(rhats_df, file = paste(model_path, "model_summaries/", "rhats_df.csv", sep = ""))
write.csv(neff_df, file = paste(model_path, "model_summaries/", "neff_df.csv", sep = ""))
write.csv(ic_df, file = paste(model_path, "model_summaries/", "ic_df.csv", sep = ""))

###### PRELIM PLOTS ######

my_offset <- 0.005

coefs_df %>%
  filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  ggplot(aes(x = quantile - my_offset, y = .value)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_pointinterval() + 
  geom_line() + 
  labs(x = "Minimum quantile", y = "Proportional change in size", title = "Lottia digitalis")

ic_df %>% 
  filter(truncate == TRUE) %>% 
  filter(.rownames == "waic" | .rownames == "looic") %>% 
  ggplot(aes(x    = quantile, 
             y    = Estimate, 
             color = .rownames, 
             ymin = Estimate - SE, 
             ymax = Estimate + SE)) +
  geom_pointrange(alpha = 0.5) + 
  labs(x = "Minimum quantile", y = "WAIC", title = "Lottia digitalis") 

