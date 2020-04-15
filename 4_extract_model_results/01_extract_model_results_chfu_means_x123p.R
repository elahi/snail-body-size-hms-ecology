################################################################################
##' @title Extract model summaries, Tegula funebralis (x123p; means)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-16
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
model_path <- "brm_models/chfu/logsize_x123p/"

# Get the most recent file, assumes that the files are ordered chronologically
fileNames <- dir(path = model_path, recursive = FALSE, pattern = ".rds")
nFiles <- length(fileNames)
fileNames

#b_final_filename <- paste(model_path, "/", fileNames[length(fileNames)], sep = "")
b_final_filename <- paste(model_path, "/", fileNames, sep = "")
b_final <- readRDS(b_final_filename)
fit <- b_final
get_variables(fit)

coefs <- fit %>%
  gather_draws(b_erapresent, b_sampleAreaWara.D, 
               b_x2z, b_x3z, 
               `b_erapresent:x2z`, `b_erapresent:x3z`) %>%
  median_qi(.width = c(0.95, 0.8)) %>% 
  mutate(variable = as.factor(.variable))

# Rename the levels of variable
variable <- factor(coefs$.variable)
levels(variable)
levels(coefs$variable) <- c("Era", "Era x Density", "Era x Tidal height", 
                            "Sample Area", 
                            "Density", "Tidal height")

## Extract model coefficients
extract_coefs <- function(fit, truncate_logic = FALSE, my_quantile = NA){
  
  library(tidyverse)
  library(tidybayes)
  
  # Make dataframe for plotting coefficients
  coefs <- fit %>%
    gather_draws(b_erapresent, b_sampleAreaWara.D, 
                 b_x2z, b_x3z, 
                 `b_erapresent:x2z`, `b_erapresent:x3z`) %>%
    median_qi(.width = c(0.95, 0.8)) %>% 
    mutate(variable = as.factor(.variable), 
           truncate = truncate_logic, 
           quantile = my_quantile)
  
  # Rename the levels of variable
  #variable <- factor(coefs$.variable)
  levels(coefs$variable) <- c("Era", "Era x Density", "Era x Tidal height", 
                              "Sample Area", 
                              "Density", "Tidal height")
  
  # Relevel the variables
  coefs <- coefs %>%
    mutate(variable = fct_relevel(variable, 
                                  "Era", "Sample Area", 
                                  "Density", "Tidal height", 
                                  "Era x Density", "Era x Tidal height"))
  
  return(coefs)
  
}

coefs_df <- extract_coefs(fit = fit, truncate_logic = FALSE, my_quantile = NA)
coefs_df

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

##### SAVE FILES #####
write.csv(coefs_df, file = paste(model_path, "model_summaries/", "coefs_df.csv", sep = ""))
write.csv(r_sq_df, file = paste(model_path, "model_summaries/", "r_sq_df.csv", sep = ""))
write.csv(rhats_df, file = paste(model_path, "model_summaries/", "rhats_df.csv", sep = ""))
write.csv(neff_df, file = paste(model_path, "model_summaries/", "neff_df.csv", sep = ""))

