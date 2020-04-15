################################################################################
##' @title Plot seawater and air temperature time-series together
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-16
##' @log 
################################################################################

## Temperature data
source("2_summarise_data/summarise_temperature_annual.R")
annual_df_long

## Years of Elahi snail samples
source("3_analyse_data/01_sbs_bayes_data.R")
dat <- dat %>% 
  mutate(species = as.character(species), 
         species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species))

# Get relevant years for each species
spYrs <- dat %>% ungroup() %>% select(species, year) %>% distinct() %>% 
  arrange(species, year) 
spYrs
gen_sp <- spYrs$species
gen_sp
genus <- vector(length = length(gen_sp))
genus
for(i in 1:length(gen_sp)){
  genus[i] <- unlist(strsplit(gen_sp[i], split = " ", fixed = TRUE))[1]
}
genus
spYrs$genus <- genus
spYrs$genus <- factor(spYrs$genus, levels = c("Tegula", "Lottia", "Littorina"))

##### TWO-PANEL PLOT #####

air_means <- annual_df_long %>% 
  group_by(dataset, metric) %>% 
  summarise(mean = mean(tempC, na.rm = TRUE)) %>% 
  filter(dataset == "Air") %>% arrange(mean)
air_means

text_df <- data.frame(x = c(rep(1937, 3)), 
                      y = air_means$mean, 
                      text1 = c("Min", "Mean", "Max"), 
                      dataset = rep("Air", 3), 
                      metric = c("minimum", "mean", "maximum"))

facet_label_text <- data.frame(x = rep(1937, 2), 
                               y = rep(24, 2), 
                               text1 = c("a", "b"), 
                               dataset = c("Air", "Seawater"))

theme_set(theme_bw() + 
            theme(panel.grid  = element_blank(), 
                  strip.background = element_blank()))

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 8, name = 'RdBu')
brewer.pal(n = 8, name = "RdBu")

col_max <- "#B2182B"
col_min <- "#2166AC"
col_mean <- "black"

spYrs <- spYrs %>% 
  mutate(species = factor(species, 
                          levels = c("Tegula funebralis", "Lottia digitalis", "Littorina keenae")))
annual_df_long %>% 
  ggplot(aes(year, tempC, color = metric)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Year") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") + 
  facet_wrap(~ dataset) + 
  scale_color_manual(values = c(col_max, col_mean, col_min)) + 
  geom_text(aes(x, y, label = text1), data = text_df, size = 3, 
            hjust = 0, fontface = "bold") + 
  geom_text(aes(x, y, label = text1, color = NULL, shape = NULL), 
            data = facet_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) +
  geom_point(aes(x = year, y = 4.5, shape = species, color = NULL), 
             data = spYrs, alpha = 1, size = 2) + 
  scale_shape_manual(values = c(1, 2, 0))
  
ggsave("figs/plot_temperature_time.pdf", height = 3, width = 6)
ggsave("figs/plot_temperature_time.jpg", height = 3, width = 6, dpi = 600)
