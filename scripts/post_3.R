

weighted_frequentist_probabilities <- read_csv('data/weighted_frequentist_probabilities.csv')

canadiens_example <-
  weighted_frequentist_probabilities %>%
  filter(Skater %in% c('BEDARD, CONNOR', 'FANTILLI, ADAM', 'MICHKOV, MATVEI', 'CARLSSON, LEO', 'SMITH, WILLIAM'), 
         rank <= 5) %>%
  group_by(Skater) %>%
  mutate(probability = cumsum(probability)) %>%
  ungroup() %>%
  mutate(probability = 1 - probability) %>%
  mutate(pick = rank + 1) %>%
  pivot_wider(id_cols = pick, names_from = Skater, values_from = probability) %>%
  filter(pick < 6) %>%
  add_row(pick = 1, 'BEDARD, CONNOR' = 1, 'FANTILLI, ADAM' = 1, 'MICHKOV, MATVEI' = 1, 
          'CARLSSON, LEO' = 1, 'SMITH, WILLIAM' = 1) %>%
  arrange(pick) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_percent(columns = 2:6)

# gtsave(canadiens_example, 
#        "draft-probabilities-3-1.png", 
#        path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',
#        expand = 0)




