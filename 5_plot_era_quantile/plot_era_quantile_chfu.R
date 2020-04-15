################################################################################
##' @title Plot era effect by quantile, Tegula funebralis
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-14
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)
library(tidybayes)
theme_set(theme_tidybayes())
library(viridis)
library(forcats)

model_path <- "brm_models/chfu/"      # CHANGE

coefs_x1p <- read.csv(
  paste(model_path, 
        "logsize_x1p/",                # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Completely pooled",
         variables = "Era", 
         description = "No ecological context", 
         model = "logsize_x1p")        # CHANGE

# coefs_x12p <- read.csv(
#   paste(model_path, 
#         "logsize_x12p/",                # CHANGE
#         "model_summaries/", 
#         "coefs_df",
#         ".csv", sep = "")) %>% 
#   mutate(pooling = "Completely pooled",
#          variables = "Era", 
#          description = "Sampling design", 
#          model = "logsize_x12p")        # CHANGE

coefs_x12p_era <- read.csv(
  paste(model_path, 
        "logsize_x12p/",                # CHANGE
        "model_summaries/", 
        "era_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Completely pooled",
         variables = "Era", 
         description = "Sampling design", 
         model = "logsize_x12p")        # CHANGE

names(coefs_x1p)
names(coefs_x12p_era)

coefs_df <- rbind(coefs_x1p, coefs_x12p_era) %>% 
  mutate(description = factor(description))

# Relevel
coefs_df <- coefs_df %>% 
  mutate(description = relevel(description, "No ecological context"))
coefs_df$description

###### PRELIM PLOTS ######

dodge <- position_dodge(width = 0.025) 

coefs_df %>%
  #filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  ggplot(aes(x = quantile, y = .value, color = description, group = .variable)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(position = dodge, alpha = 0.5) + 
  geom_pointinterval(position = dodge) + 
  labs(x = "Lower size threshold (quantile)", y = "Proportional change in size", 
       title = "Tegula funebralis") + 
  scale_color_viridis_d(begin = 0, end = 0.75) + 
  theme(legend.justification=c(1, 0),
        legend.position = c(0.95, 0.05),
        legend.background = element_blank(),
        legend.key = element_blank(), 
        legend.box = "horizontal") +
  guides(linetype = guide_legend(override.aes = list(size = 10))) + 
  #theme(legend.position = "right") +
  theme(legend.title = element_blank())

#ggsave("figs/plot_model_coefs_chfu.png", height = 3.5, width = 5)



