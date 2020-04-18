################################################################################
##' @title Plot era effect by quantile
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-11-05
################################################################################

## Make Tegula panel
source("5_plot_era_quantile/plot_era_quantile_chfu.R")
chfu_df <- coefs_df %>% 
  filter(truncate == TRUE) %>% 
  mutate(species = "Tegula funebralis")
names(chfu_df)

## Make Lottia panel
source("5_plot_era_quantile/plot_era_quantile_lodi.R")
lodi_df <- coefs_df %>%   
  filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  mutate(species = "Lottia digitalis")
names(lodi_df)

## Make Littorina panel
source("5_plot_era_quantile/plot_era_quantile_like.R")
like_df <- coefs_df %>% 
  filter(variable == "Era") %>% 
  filter(truncate == TRUE) %>% 
  mutate(species = "Littorina keenae")
names(like_df)

## Combine all 3
df <- rbind(chfu_df, lodi_df, like_df)
names(df)

df <- df %>% 
  mutate(description2 = ifelse(model == "logsize_x1p", 
                               "Field-agnostic",
                               "Field-contextualized"), 
         description2 = relevel(factor(description2), "Field-agnostic"))

## Remove some quantiles for clarity
p <- c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
df <- df[df$quantile %in% p, ]

##### PLOT 3 PANELS #####

# Relevel
df <- df %>% 
  mutate(species = (factor(species, 
                           levels = c("Tegula funebralis", 
                           "Lottia digitalis", 
                           "Littorina keenae"))))

facet_panels <- df %>% 
  filter(!is.na(species)) %>% 
  select(species) %>% distinct() %>%
  arrange(species)

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(letters)[1:3]) %>% 
  arrange(facet_labels)

df <- df %>% 
  left_join(., facet_panels, by = c("species"))

df2 <- df %>% 
  filter(model == "logsize_x123" | model == "logsize_x1p" | model == "logsize_x12p")

# Add in text for Tegula panel
tegula_text <- data.frame(
  species = "Tegula funebralis", 
  site = c("Exposed", "Sheltered"), 
  x = c(0.27, 0.3), 
  y = c(-0.05, -0.25)
)

tegula_text

## Use L digitalis / L austrodigitalis

df3 <- df2 %>% 
  mutate(species = ifelse(species == "Lottia digitalis", 
                          "Lottia digitalis / L. austrodigitalis", 
                          as.character(species))) %>% 
  mutate(species = (factor(species, 
                           levels = c("Tegula funebralis", 
                                      "Lottia digitalis / L. austrodigitalis", 
                                      "Littorina keenae"))))

facet_panels2 <- facet_panels %>% 
  mutate(species = ifelse(species == "Lottia digitalis", 
                          "Lottia digitalis / L. austrodigitalis", 
                          as.character(species))) %>% 
  mutate(species = (factor(species, 
                           levels = c("Tegula funebralis", 
                                      "Lottia digitalis / L. austrodigitalis", 
                                      "Littorina keenae"))))

theme_set(theme_bw(base_size = 10) + 
            theme(strip.background = element_blank(), 
                  strip.text = element_text(face = "italic", size = 8), 
                  panel.grid = element_blank())) 
dodge <- position_dodge(width = 0.025) 

df3 %>%
  filter(.width == 0.95) %>% 
  ggplot(aes(x = quantile, y = .value, 
             color = description2, shape = description2, model = variable)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = 0.3) +
  geom_line(aes(), position = dodge, alpha = 0.5, size = 0.3, 
            show.legend = FALSE) + 
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
                data = subset(df3, .width == 0.80), 
                size = 0.9, 
                position = dodge, width = 0, alpha = 0.75) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
                size = 0.45, 
                position = dodge, width = 0, alpha = 0.75) +
  geom_point(position = dodge, size = 2.5,
             show.legend = TRUE, alpha = 1) +
  labs(x = "Lower size threshold (quantile)", 
       y = "Proportional change in size") + 
  theme(legend.box = "horizontal", 
        legend.background = element_blank()) +
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0.01, 0.01)) + 
  facet_wrap(~ species) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("black", "red")) + 
  scale_shape_manual(values = c(20, 18)) + 
  coord_cartesian(ylim = c(-0.6, 0.25)) + 
  geom_text(data = facet_panels2, aes(0, 0.25, label = facet_labels), 
            inherit.aes = FALSE, size = 4, nudge_x = 0.01, nudge_y = -0.02) + 
  geom_text(data = tegula_text, aes(x = x, y = y, label = site), 
            inherit.aes = FALSE, color = "red", size = 2.5)

## Figure 2
ggsave("figs/plot_era_quantile_color2.pdf", height = 2.5, width = 6)
ggsave("figs/plot_era_quantile_color2.jpg", height = 2.5, width = 6, dpi = 600)

