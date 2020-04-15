################################################################################
##' @title Plot model coefficients for each species
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-16
##' @log 
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)
library(tidybayes)

## Tegula
model_path <- "brm_models/chfu/"      # CHANGE
coefs_x123_chfu <- read.csv(
  paste(model_path, 
        "logsize_x123p/",              # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Completely pooled", 
         variables = "Era, density, tidal height", 
         description = "Sampling design, density, tidal height", 
         model = "logsize_x123") %>% 
  mutate(species = "Tegula funebralis")

## Lottia
model_path <- "brm_models/lodi/"      # CHANGE
coefs_x123_lodi <- read.csv(
  paste(model_path, 
        "logsize_x123/",              # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Partially pooled", 
         variables = "Era, density, tidal height", 
         description = "Sampling design, density, tidal height", 
         model = "logsize_x123") %>% 
  mutate(species = "Lottia digitalis / L. austrodigitalis")

## Littorina
model_path <- "brm_models/like/"      # CHANGE
coefs_x123_like <- read.csv(
  paste(model_path, 
        "logsize_x123/",              # CHANGE
        "model_summaries/", 
        "coefs_df",
        ".csv", sep = "")) %>% 
  mutate(pooling = "Partially pooled", 
         variables = "Era, density, tidal height", 
         description = "Sampling design, density, tidal height", 
         model = "logsize_x123") %>% 
  mutate(species = "Littorina keenae")

coefs_df <- rbind(coefs_x123_chfu, coefs_x123_lodi, coefs_x123_like) %>% 
  mutate(species = factor(species, levels = c("Tegula funebralis", "Lottia digitalis / L. austrodigitalis", 
                                              "Littorina keenae")))

levels(coefs_df$variable)

coefs_df <- coefs_df %>% 
  mutate(variable = factor(variable, levels = rev(c("Era", "Density", "Tidal height", 
                                                "Era x Density", 
                                                "Era x Tidal height", 
                                                "Sample Area"))))

###### PLOT ######

dodge <- position_dodge(width = 0.7) 

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic", size = 9)))

coefs_df %>%
  filter(species == "Tegula funebralis" | 
           species == "Lottia digitalis / L. austrodigitalis" & quantile == 0 | 
           species == "Littorina keenae" & quantile == 0) %>% 
  ggplot(aes(x = variable, y = .value)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_pointinterval(show.legend = FALSE, fatten_point = 1.2) + 
  facet_wrap(~ species) + 
  coord_flip() + 
  labs(x = "", y = "Standardized coefficient") + 
  theme(legend.position = c(0.5, 0.001), legend.justification = c(0.5, 0.001)) + 
  theme(legend.title = element_blank()) + 
  theme(legend.box = "horizontal") + 
  theme(legend.background = element_blank()) + 
  theme(legend.text = element_text(size = 10)) + 
  scale_color_manual(#values = my_colors, 
                     name = "Lower size\nthreshold\n(quantile)", 
                     guide = guide_legend(reverse = TRUE))

## Figure S3
ggsave("figs/plot_coefs_x123_austro.pdf", height = 3.5, width = 8) 


