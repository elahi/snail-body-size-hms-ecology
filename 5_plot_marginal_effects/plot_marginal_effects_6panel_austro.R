################################################################################
##' @title Plot marginal effects, 6 panels
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-17
################################################################################

##### PACKAGES, DATA #####

source("5_plot_marginal_effects/plot_marginal_effects_chfu.R")
source("5_plot_marginal_effects/plot_marginal_effects_lodi.R")
source("5_plot_marginal_effects/plot_marginal_effects_like.R")

library(cowplot)

##### RENAME FOR AUSTRODIGITALIS #####
unique(d_lodi$species)
d_lodi <- d_lodi %>% 
  mutate(species = "Lottia digitalis / L. austrodigitalis")

pi_lodi_density <- pi_lodi_density %>%   
  mutate(species = "Lottia digitalis / L. austrodigitalis")

pi_lodi_tide <- pi_lodi_tide %>%   
  mutate(species = "Lottia digitalis / L. austrodigitalis")

##### PLOTTING OPTIONS #####

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic", size = 9), 
                  panel.grid = element_blank())) 

density_axis_X <- xlab(expression(paste("Density (no. ", m^-2, ")"))) 
tide_axis_X <- xlab("Tidal height (m)")

past_col = "red"
present_col = "black"
my_letter = "A"
# past_col <- "#ef8a62"
# present_col <- "#67a9cf"

plot_size <- function(my_data, x = "density_m2", y = "size_log", 
                      past_color = past_col, present_color = present_col, my_letter = "a"){
  
  # Plot data
  my_data %>% 
    ggplot(aes_string(x, y, shape = "era", color = "era")) + 
    geom_point(alpha = 0.75, size = 1) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c(past_color, present_color)) + 
    scale_shape_manual(values = c(1, 1)) + 
    facet_wrap(~ species, scales = "free") + 
    geom_point(data = subset(my_data, era == "past"), alpha = 0.75, size = 1) + 
    ylab("Log size (mm)") 
}

plot_size_tegula <- function(my_data, x = "density_m2", y = "size_log", 
                      past_color = past_col, present_color = present_col, my_letter = "a"){
  
  # Plot data
  my_data %>% 
    ggplot(aes_string(x, y, shape = "sampleArea", color = "era")) + 
    geom_point(alpha = 0.75, size = 1) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c(past_color, present_color)) + 
    scale_shape_manual(values = c(2, 1)) + 
    facet_wrap(~ species, scales = "free") + 
    geom_point(data = subset(my_data, era == "past"), alpha = 0.75, size = 1) + 
    ylab("Log mean size (mm)") 
}

##### TEGULA #####

label_x <- max(d_chfu$density_m2) * 0.99
label_y <- max(d_chfu$size_log) * 0.99

p1 <- plot_size_tegula(my_data = d_chfu) + 
  geom_lineribbon(data = pi_chfu_density, 
                  aes(y = .value, group = interaction(era, sampleArea)), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  density_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "a") 

p1


##### LOTTIA #####

label_x <- min(d_lodi$density_m2) * 1.01
label_y <- max(d_lodi$size_log) * 0.99

p2 <- plot_size(my_data = d_lodi) + 
  geom_lineribbon(data = pi_lodi_density, aes(y = .value), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  density_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "b") 

p2

##### LITTORINA #####

label_x <- min(d_like$density_m2) * 1.01
label_y <- max(d_like$size_log) * 1.05

p3 <- plot_size(my_data = d_like) + 
  geom_lineribbon(data = pi_like_density, aes(y = .value), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  density_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "c")

p3

##### TEGULA #####

label_x <- max(d_chfu$tideHTm) * 0.99
label_y <- max(d_chfu$size_log) * 1.0

p4 <- plot_size_tegula(my_data = d_chfu, x = "tideHTm") + 
  geom_lineribbon(data = pi_chfu_tide, 
                  aes(x = tideHTm, y = .value, group = interaction(era, sampleArea)), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  tide_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "d") 

p4


##### LOTTIA #####

label_x <- min(d_lodi$tideHTm) * 1
label_y <- max(d_lodi$size_log) * 0.99

p5 <- plot_size(my_data = d_lodi, x = "tideHTm_offset") + 
  geom_lineribbon(data = pi_lodi_tide, aes(x = tideHTm_offset, y = .value), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  tide_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "e") 

p5

##### LITTORINA #####

label_x <- min(d_like$tideHTm) * 1.01
label_y <- max(d_like$size_log) * 1.03

p6 <- plot_size(my_data = d_like, x = "tideHTm_offset") + 
  geom_lineribbon(data = pi_like_tide, aes(x = tideHTm, y = .value), 
                  show.legend = FALSE, alpha = 0.5) +
  scale_fill_brewer(palette = "Greys") + 
  tide_axis_X +
  annotate(geom = "text", x = label_x, y = label_y, label = "f")

p6

##### COMBINE PLOTS #####

add_my_legend <- theme(legend.position = c(0.995, 0.995), 
                        legend.justification = c(0.995, 0.995)) + 
  theme(legend.title = element_blank(), 
        legend.margin=margin(c(0, 4, 4, 4))) +
  theme(legend.key = element_rect(fill = "white"), 
        legend.key.size = unit(0.1, "cm"), 
        legend.text = element_text(size = 8)) + 
  theme(legend.background = element_rect(color = "black", size = 0.1))

plot_size_covariate_6panel <- plot_grid(p1, 
                                        p2 + add_my_legend, 
                                        p3, 
                                        p4, 
                                        p5, 
                                        p6,
                                        ncol = 3)

plot_size_covariate_6panel

## Figure 3
save_plot("figs/plot_marginal_effects_6panel_austro.pdf", plot_size_covariate_6panel, 
          ncol = 3, nrow = 2, base_height = 2.5, base_width = 2.5)

save_plot("figs/plot_marginal_effects_6panel_austro.jpg", plot_size_covariate_6panel, 
          ncol = 3, nrow = 2, base_height = 2.5, base_width = 2.5, dpi = 600)
