################################################################################
##' @title Plot era effect by quantile, Littorina keenae
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-12
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)
library(tidybayes)
theme_set(theme_tidybayes())
library(viridis)
model_path <- "brm_models/like/"      # CHANGE

coefs_x1 <- read.csv(
  paste(model_path, 
        "logsize_x1/",                # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Partially pooled",
         variables = "Era", 
         description = "Sampling design", 
         model = "logsize_x1")        # CHANGE

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

coefs_x123 <- read.csv(
  paste(model_path, 
        "logsize_x123/",              # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Partially pooled", 
         variables = "Era, density, tidal height", 
         description = "Sampling design, density, tidal height", 
         model = "logsize_x123")      # CHANGE

coefs_x123p <- read.csv(
  paste(model_path, 
        "logsize_x123p/",              # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Completely pooled",
         variables = "Era, density, tidal height", 
         description = "Density, tidal height", 
         model = "logsize_x123p")      # CHANGE


coefs_df <- rbind(coefs_x1, coefs_x1p, coefs_x123, coefs_x123p) %>% 
  mutate(description = factor(description))

# Relevel
library(forcats)
coefs_df <- coefs_df %>% 
  mutate(description = relevel(description, "No ecological context"))
coefs_df$description

###### PRELIM PLOTS ######

dodge <- position_dodge(width = 0.025) 

coefs_df %>%
  filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  ggplot(aes(x = quantile, y = .value, color = pooling)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(position = dodge, alpha = 0.5) + 
  geom_pointinterval(position = dodge) + 
  facet_wrap(~ variables) + 
  labs(x = "Minimum quantile", y = "Proportional change in size", title = "Littorina keenae")


coefs_df %>%
  filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  #filter(model != "logsize_x123p") %>%
  ggplot(aes(x = quantile, y = .value, color = description, shape = pooling)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(position = dodge, alpha = 1, size = 0.5) + 
  geom_point(position = dodge, show.legend = FALSE) + 
  geom_pointinterval(position = dodge, show.legend = FALSE) + 
  labs(x = "Lower size threshold (quantile)", y = "Proportional change in size", title = "Littorina keenae") + 
  scale_color_viridis_d(begin = 0, end = 0.75) + 
  theme(legend.justification=c(1, 0),
        legend.position = c(0.95, 0.05),
        legend.background = element_blank(),
        legend.key = element_blank(), 
        legend.box = "horizontal") +
  guides(linetype = guide_legend(override.aes = list(size = 10))) + 
  #theme(legend.position = "right") +
  theme(legend.title = element_blank())

#ggsave("figs/plot_model_coefs_like.png", height = 3.5, width = 5)



