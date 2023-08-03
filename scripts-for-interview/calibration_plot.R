
# Create a calibration plot to assess the calibration of the model predictions

calib_plot <-
  weighted_frequentist_probabilities %>%
  left_join(tibble(Skater = live_picks, rank = 1:64, pick = 1), by = c('Skater', 'rank')) %>%
  mutate(pick = replace_na(pick, 0)) %>%
  group_by(Skater) %>%
  mutate(probability = cumsum(probability), pick = cumsum(pick)) %>%
  ungroup() %>%
  filter(rank <= 64) %>%
  filter(probability > 0, probability < 1)

calib_plot %>%
  mutate(bins = .bincode(calib_plot$probability,  seq(-0.01, 1.01, 0.05))) %>%
  group_by(bins) %>%
  summarize(pred = mean(probability), truth = mean(pick)) %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = 'dashed', col = 'white') +
  geom_line(aes(pred, truth), col = single_color) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black')
  ) +
  labs(x = '\nPredicted Probability', y = 'Observed Probability\n')
  