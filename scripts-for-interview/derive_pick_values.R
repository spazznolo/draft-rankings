
predicted_prospect_values <-
  tibble(
    bedard = rnorm(100000, 24, 3),
    fantilli = rnorm(100000, 19, 4),
    michkov = rnorm(100000, 17.5, 7),
    carlsson = rnorm(100000, 14, 4.5),
    smith = rnorm(100000, 12, 5))

predicted_prospect_values %>%
  pivot_longer(everything(), names_to = 'prospect', values_to = 'war') %>%
  mutate(war = ifelse(war < 0, 0, war)) %>%
  ggplot() +
  geom_density(aes(war, fill = prospect), alpha = 0.75) +
  scale_fill_manual(values = c('lightblue', "#78B7C5", "#EBCC2A", "yellowgreen", "#F21A00")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

predicted_prospect_values <-
  tibble(
    pre_draft_rank = 1:5,
    value_mean = c(24, 19, 17.5, 14, 12),
    value_sd = c(3, 4, 7, 4.5, 5))

top_5_probs <-
  weighted_frequentist_probabilities %>%
  mutate(rank = rank + 1)%>%
  left_join(skater_dictionary %>% select(Skater, pre_draft_rank), by = 'Skater') %>%
  left_join(predicted_prospect_values, by = 'pre_draft_rank') %>%
  arrange(pre_draft_rank) %>%
  group_by(Skater) %>%
  mutate(
    probability = cumsum(probability),
    probability = 1 - probability) %>%
  ungroup() %>% 
  group_by(rank) %>% 
  mutate(
    remaining_probability = cumsum(probability),
    probability = ifelse(remaining_probability > 1, 1 - lag(remaining_probability), remaining_probability),
    probability = ifelse(probability < 0, 0, probability)) %>%
  ungroup() %>%
  filter(rank <= 5, probability > 0) %>%
  mutate(picks = 10000*probability) %>%
  add_row(Skater = 'BEDARD, CONNOR', rank = 1, picks = 10000, value_mean = 24, value_sd = 3) 

top_5_probs %>%
  rowwise() %>%
  mutate(sample = list(rnorm(picks, value_mean, value_sd))) %>%
  unnest(sample) %>%
  ggplot() +
  geom_density_ridges(aes(x = sample, y = rank, fill = factor(rank)), alpha = 0.75, bandwidth = 1) +
  scale_fill_manual(values = c('lightblue', "#78B7C5", "#EBCC2A", "yellowgreen", "#F21A00")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none') +
  labs(x = '\n7-year predicted WAR', y = 'Pick\n')


compare_picks_4_5 <-
  top_5_probs %>%
  rowwise() %>%
  mutate(sample = list(rnorm(picks, value_mean, value_sd))) %>%
  unnest(sample) %>%
  filter(rank %in% 4:5) %>%
  group_by(rank) %>%
  mutate(idx = 1:n()) %>%
  slice(1:10000) %>%
  ungroup() %>%
  select(idx, rank, sample) %>%
  pivot_wider(id_cols = idx, names_from = rank, values_from = sample) %>%
  clean_names() %>%
  mutate(diff = x4 - x5)

compare_picks_4_5 %>%
  ggplot() +
  geom_vline(xintercept = mean(compare_picks_4_5$diff, na.rm = TRUE), alpha = 0.5, col = 'white', linetype = 'dashed', lwd = 1) +
  geom_density(aes(diff), alpha = 0.75, fill = single_color) +
  scale_y_continuous(labels = scales::percent) +
  annotate(
    "text", x = 22, y = 0.045, col = 'white', size = 3, 
    label = paste0("Expected Difference:\n", round(mean(compare_picks_4_5$diff, na.rm = TRUE), 2)," WAR")) +
  annotate(
    "text", x = -22, y = 0.045, col = 'white', size = 3, 
    label = paste0("Expected Win Rate:\n", round(100*mean(compare_picks_4_5$diff > 0, na.rm = TRUE), 2)," %")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none',
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

