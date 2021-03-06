################################################################################
##' @title Function to choose which size data to use
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-08-21
##' @log Add a log here
################################################################################


choose_size_data <- function(method = 'repeated'){
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  
  # load data - repeated size bins
  if(method == "repeated"){
    dat <- read.csv("./output/sbsMaster.csv", na.strings = "NA", 
             stringsAsFactors = FALSE) %>%
      select(-c(X, row))
  }

  # load data - normally approximated sizes
  if(method == "normal"){
    dat <- read.csv("./output/sbsMaster_norm.csv", na.strings = "NA", 
             stringsAsFactors = FALSE) %>%
      select(-c(X, row))
  }
  
  # load data - uniformly approximated sizes
  if(method == "uniform"){
    dat <- read.csv("./output/sbsMaster_unif.csv", na.strings = "NA", 
                    stringsAsFactors = FALSE) %>%
      select(-c(X, row))
  }
  
  # create numeric lat-long columns
  dat$lat2 <- as.numeric(substr(dat$lat, 1, 8))
  dat$long2 <- as.numeric(paste("-", substr(dat$long, 1, 9), sep = ""))
  dat$LL <- with(dat, paste(lat2, long2, sep = ","))
  
  # Create new column called 'sampleArea' based on 
  # the areas that were resampled in modern surveys
  dat$sampleArea <- as.factor(ifelse(dat$sp == "CHFU", 
                                     paste(dat$site), 
                                     paste(dat$site, dat$nest1, sep = "_")))
  
  # Define sampling units (Lottia is exception here)
  dat2 <- dat %>% 
    mutate(sampleUnit = ifelse(era == "present" & sp == "LODI", 
                               paste(site, nest1, nest2, sep = "_"), sampleUnit))
  
  # Get mean tidal heights for sample areas
  tidalHTdf <- dat2 %>% filter(!is.na(tideHTm)) %>% 
    group_by(sampleArea) %>% 
    summarise(sample_area_tidal_ht = mean(tideHTm))
  
  # Join sample area tidal heights
  dat2 <- tidalHTdf %>% select(sampleArea, sample_area_tidal_ht) %>% 
    inner_join(dat2, ., by = "sampleArea")
  
  # Get proportion of max size
  dat2 <- dat2 %>% group_by(species) %>% 
    mutate(size_prop = size1mm/max(size1mm, na.rm = TRUE)) %>% 
    ungroup()
  
  # Simplify dataframe
  dat2 <- dat2 %>% 
    select(-c(lat, long)) %>% 
    rename(lat = lat2, long = long2) 
  
  if(method == "repeated"){
    dat2 <- dat2 %>% mutate(year = lubridate::year(dmy(date)))
  }
  
  return(dat2)
}

