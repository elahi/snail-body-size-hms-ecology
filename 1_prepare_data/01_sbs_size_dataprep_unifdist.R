################################################################################
##' @title Prepare gastropod size-frequency data using a uniform distribution
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-08-20
##' @log 
################################################################################

##### PACKAGES, DATA #####

library(dplyr)
library(lubridate)

# Functions to convert histogram data to vector of raw sizes
source("R/convert_histo_to_raw.R")

# Load modern data
mod <- read.csv("output/snail_body_size_hms_modern.csv", na.strings = c("NA")) %>% select(-X)
mod %>% distinct(era, year, date)

# Load historic data
childs <- read.csv("./data/childs_raw.csv", na.strings = c("NA", ""))
wara <- read.csv("./data/wara_raw.csv", na.strings = c("NA", ""))
hexPast <- read.csv("./data/hexter_raw.csv", na.strings = c("NA", "")) %>% select(-X)

# need to expand these size-frequency tables
childs
wara # only resampled area B and area D

##### CHILDS #####
# Childs - Littorina keenae
childsA <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_A, 
                            size_interval = 1, distribution = "uniform")
childsB <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_B, 
                            size_interval = 1, distribution = "uniform")
childsC <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_C, 
                            size_interval = 1, distribution = "uniform")
childsD <- get_random_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_D, 
                            size_interval = 1, distribution = "uniform")

mod %>% filter(sp == "LIKE") %>% glimpse()

childsPast <- data.frame(row = "", 
                         species = "Littorina.keenae", 
                         sp = "LIKE", 
                         site = "HighRock", 
                         era = "past", 
                         date = "1947-06-14",
                         nest1 = c(rep("zoneA", length(childsA)), 
                                   rep("zoneB", length(childsB)), 
                                   rep("zoneC", length(childsC)), 
                                   rep("zoneD", length(childsD))), 
                         nest2 = "area1", 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = "", 
                         size1mm = round(c(childsA, childsB, childsC, childsD), 1),
                         habitat = "bare rock", 
                         tideHTm = NA, 
                         lat = "36.62183N", 
                         long = "121.90516W", 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
                         )

childsPast <- childsPast %>% mutate(year = lubridate::year(date))
min(childsPast$size1mm)

# Compare the uniform distribution with a simpler approach by repeating size bins
childsA <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_A)
childsB <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_B)
childsC <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_C)
childsD <- repeat_sizes(size_bin_vector = childs$length_mm, count_vector = childs$count_D)

childs_past_repeat <- c(childsA, childsB, childsC, childsD)
plot(density(childs_past_repeat), main = "")

lines(density(childsPast$size1mm), col = "red")

##### WARA #####
wara
### Wara - Chlorostoma funebralis
waraB <- get_random_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_B, 
                          size_interval = 2, distribution = "uniform")
waraD <- get_random_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_D, 
                          size_interval = 2, distribution = "uniform")

mod %>% filter(sp == "CHFU") %>% glimpse()

waraPast <- data.frame(row = "", 
                       species = "Chlorostoma.funebralis", 
                         sp = "CHFU", 
                         site = c(rep("Wara.B", length(waraB)), 
                                rep("Wara.D", length(waraD))), 
                         era = "past", 
                         date = "1963-06-01", 
                         nest1 = NA, 
                         nest2 = NA, 
                         nest3 = NA, 
                         nest3note = NA, 
                         sampleUnit = NA, 
                         size1mm = round(c(waraB, waraD), 1), 
                         habitat = NA, 
                         tideHTm = NA, 
                         lat = NA,  
                         long = NA, 
                         Shaw_hab = NA, 
                         notes = "", 
                         notes2 = ""
)

waraPast <- waraPast %>% mutate(year = lubridate::year(date))
min(waraPast$size1mm)

# Compare the uniform distribution with a simpler approach by repeating size bins
waraB <- repeat_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_B)
waraD <- repeat_sizes(size_bin_vector = wara$length_mm, count_vector = wara$count_D)
wara_past_repeat <- c(waraB, waraD)
plot(density(wara_past_repeat), main = "")
lines(density(waraPast$size1mm), col = "red")

##### COMBINE MODERN AND PAST DATASETS #####
sbsMaster <- rbind(mod, childsPast, waraPast, hexPast)
unique(sbsMaster$date)
glimpse(sbsMaster)
summary(sbsMaster)
sbsMaster$row <- seq(1:length(sbsMaster$sp))
write.csv(sbsMaster, "output/sbsMaster_unif.csv")
