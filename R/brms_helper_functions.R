################################################################################
##' @title Functions to run brm models per species and quantile removed
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-09
##' @log 
################################################################################

# Example model_path
# "brm_models/lodi/logsize_x123/"

# dat <- hexDF
# species_abbrev <- "lodi"
# CHAINS <- 1
# CORES <- 4
# SEED <- 101
# ITER <- 1000
# my_p <- 0.2

run_brm_truncated <- function(dat, p_vector, model_path){
  
  for(i in 1:length(p_vector)){
    
    my_p = p_vector[i]
    
    ## Truncate data
    statDat <- truncate_data(dat, era = "past", quant = my_p, filter_data = TRUE)
    
    ## Prepare dataset for stats
    statDat <- statDat %>%
      mutate(x1z = ifelse(x1 == 0, -0.5, 0.5),
             x2z = as.numeric(scale(x2)),
             x3z = as.numeric(scale(x3)),
             group_j = as.integer(as.factor(sampleUnit)),
             obs_id = seq(1:n()))
    
    fit <- readRDS(file = paste(model_path, "b.rds", sep = ""))
    fit_new <- update(fit, newdata = statDat, recompile = FALSE, 
                      file = paste(model_path, "b_", my_p*100, sep = ""))
  }
}

## Function to extract model coefficients

extract_coefs <- function(fit, truncate_logic = FALSE, my_quantile = NA, 
                          x123 = TRUE){
  
  library(tidyverse)
  library(tidybayes)
  
  if(x123 == TRUE){
    # Make dataframe for plotting coefficients
    coefs <- fit %>%
      gather_draws(b_erapresent, b_x2z, b_x3z, `b_erapresent:x2z`, `b_erapresent:x3z`) %>%
      median_qi(.width = c(0.95, 0.8)) %>% 
      mutate(variable = as.factor(.variable), 
             truncate = truncate_logic, 
             quantile = my_quantile)
    
    # Rename the levels of variable
    variable <- factor(coefs$.variable)
    levels(coefs$variable) <- c("Era", "Era x Density", "Era x Tidal height", "Density", "Tidal height")
    
    # Relevel the variables
    coefs <- coefs %>%
      mutate(variable = fct_relevel(variable, "Era", "Density", "Tidal height", 
                                    "Era x Density", "Era x Tidal height"))
    
    return(coefs)
  }
  
  if(x123 == FALSE){
    # Make dataframe for plotting coefficients
    coefs <- fit %>%
      gather_draws(b_erapresent) %>%
      median_qi(.width = c(0.95, 0.8)) %>% 
      mutate(variable = as.factor(.variable), 
             truncate = truncate_logic, 
             quantile = my_quantile)
    
    # Rename the levels of variable
    variable <- factor(coefs$.variable)
    levels(coefs$variable) <- c("Era")
    
    return(coefs)
  }
}

## Function to extract fitted and predicted draws using tidybayes

fit_n_predict <- function(dat, model){
  
  library(tidybayes)
  library(dplyr)
  
  ## Model fits
  y_fitted <- add_fitted_draws(newdata = dat, model = model)
  y_fitted <- y_fitted %>% 
    mutate(fitted_median = median(.value), 
           .residual = size_log - .value)
  
  ## Predict data for each iteration
  y_new <- add_predicted_draws(newdata = dat, model = model)
  
  y_fitted$.prediction <- y_new$.prediction
  
  return(y_fitted)

}

## Function to calculate Bayesian p-values, using the output from fit_n_predict
calculate_bpv <- function(dat){
  
  dat <- dat %>% 
    mutate(sq_error_data = (size_log - fitted_median)^2, 
           sq_error_new = (.prediction - fitted_median)^2, 
           I_k = ifelse(sq_error_new > sq_error_data, 1, 0))
  
  bpv_discrepancy <- sum(dat$I_k) / length(dat$I_k)
  
  return(bpv_discrepancy)
}


