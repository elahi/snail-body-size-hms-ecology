################################################################################
##' @title Plot snail size-frequency distributions
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-16
##' @log 
################################################################################

##### LOAD PACKAGES, DATA #####

library(grid)
library(purrr)
library(viridis)
source("3_analyse_data/01_sbs_bayes_data.R")
source("R/truncate_data.R")

# Do not remove any data; rename species
dat2 <- dat_dens %>% 
  mutate(species = as.character(species), 
         species = ifelse(species == "Chlorostoma funebralis", "Tegula funebralis", species), 
         species = factor(species, levels = c("Tegula funebralis", "Lottia digitalis", "Littorina keenae")))

## My quantiles for size threshold
#p <- seq(0, 0.5, by = 0.05)
p <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p_names <- map_chr(p, ~paste0(.x * 100))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

p_df <- dat2 %>% 
  group_by(era, species) %>% 
  summarize_at(vars(size1mm), funs(!!!p_funs)) %>% 
  ungroup()

p_df_long <- p_df %>% 
  gather(key = percentile, value = size1mm, `0`:`50`) %>% 
  mutate(percentile = as.numeric(percentile))

p_df_long_past <- p_df_long %>% filter(era == "past")

## Plot raw data with size threshold
my_quantile <- 0.5
dat2 <- truncate_data(statDat, era = "past", quant = my_quantile, filter_data = FALSE)
dat2 %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  facet_grid(era ~ species) + 
  geom_vline(aes(xintercept = size_threshold), color = "red") 


# plotting functions
source("./R/multiplotF.R")

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic")))

##### SEPARATE PANEL WITH MULTIPLOT #####

## Make separate dataframes

# Littorina keenae 
childsDF <- droplevels(filter(dat2, sp == "LIKE"))
childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")
p_df_childs <- p_df_long_past %>% filter(species == "Littorina keenae")

# Chlorostoma funebralis
waraDF <- droplevels(filter(dat2, sp == "CHFU"))
waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")
p_df_wara <- p_df_long_past %>% filter(species == "Tegula funebralis")

# Lottia digitalis
hexDF <- droplevels(filter(dat2, sp == "LODI"))
hexPast <- hexDF %>% filter(era == "past")
hexPres <- hexDF %>% filter(era == "present")
p_df_hex <- p_df_long_past %>% filter(species == "Lottia digitalis")

# Basic function to plot histogram for any subset of data

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 8, name = 'RdBu')
brewer.pal(n = 8, name = "RdBu")

display.brewer.pal(n = 8, name = 'RdBu')
brewer.pal(n = 8, name = "RdBu")

plot_histo_panel <- function(df, bin_width = 1, quantile_df) {
  
  p <- ggplot(df,  aes(x = size1mm)) +
    geom_vline(mapping = aes(
      xintercept = size1mm, 
      color = percentile), 
      data = quantile_df,
      linetype = "solid", size = 0.5, alpha = 0.65, show.legend = FALSE) + 
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = bin_width, 
                   color = "white", fill = "black") +
    #scale_colour_gradient(low = "black", high = "lightgray") + 
    scale_color_gradient(low = '#B2182B', high = '#F4A582') + 
    #scale_color_distiller(values = c(0, 1)) + 
    #scale_color_viridis_c(begin = 0, end = 0.75) + 
    xlab("Size (mm)") + ylab("Proportion")
    
  return(p)
  
}

plot_histo_panel(waraPast, quantile_df = p_df_wara)
  
# Need to customize for each panel:

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic")))

fig1a <- plot_histo_panel(waraPast, bin_width = 2, quantile_df = p_df_wara) + 
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  annotate("text", label = "1963\nn = 817", 
           x = 32, y = 0.31, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "a", 
           x = 2, y = 0.31, vjust = 1, hjust = -0.05) + 
  ggtitle("Tegula funebralis") + 
  theme(plot.title = element_text(size = 9, face = "italic", 
                                  hjust = 0.5, vjust = 0.9)) 

fig1b <- plot_histo_panel(waraPres, bin_width = 2, quantile_df = p_df_wara) + 
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  annotate("text", label = "2014\nn = 5646", 
           x = 32, y = 0.31, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "b", 
           x = 2, y = 0.31, vjust = 1, hjust = -0.05) 

fig1c <- plot_histo_panel(hexPast, quantile_df = p_df_hex) + 
  scale_x_continuous(limits = c(4, 25)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  annotate("text", label = "1950\nn = 493", 
           x = 25, y = 0.2, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "c", 
           x = 4, y = 0.2, vjust = 1, hjust = -0.05) + 
  ggtitle("Lottia digitalis / L. austrodigitalis") + 
  theme(plot.title = element_text(size = 9, face = "italic", 
                                  hjust = 0.5, vjust = 0.9))

fig1c

fig1d <- plot_histo_panel(hexPres, quantile_df = p_df_hex) + 
  scale_x_continuous(limits = c(4, 25)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2015\nn = 605", 
           x = 25, y = 0.2, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "d", 
           x = 4, y = 0.2, vjust = 1, hjust = -0.05) 

fig1e <- plot_histo_panel(childsPast, quantile_df = p_df_childs) + 
  scale_x_continuous(limits = c(1, 21)) +
  scale_y_continuous(limits = c(0, 0.23)) +
  annotate("text", label = "1947\nn = 682", 
              x = 21, y = 0.23, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "e", 
           x = 1, y = 0.23, vjust = 1, hjust = -0.05) + 
  ggtitle("Littorina keenae") + 
  theme(plot.title = element_text(size = 9, face = "italic", 
                                  hjust = 0.5, vjust = 0.9))
  
fig1f <- plot_histo_panel(childsPres, quantile_df = p_df_childs) + 
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2014\nn = 733", 
           x = 21, y = 0.2, size = 3.2, vjust = 1, hjust = 1) +
  annotate("text", label = "f", 
           x = 1, y = 0.2, vjust = 1, hjust = -0.05) 

##### SAVE MULTI PANEL PLOT #####

## Figure 1

# save as 7 x 4 pdf
pdf("figs/plot_size_frequency_austro.pdf", 7, 4)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

# save as 7 x 4 jpg
jpeg("figs/plot_size_frequency_austro.jpg", 7, 4, units = "in", res = 600)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	
