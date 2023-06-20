
example_ranking <-
  read_csv('data/draft_rankings_7.csv') %>%
  select(1:10) %>%
  slice(1:50) %>%
  mutate(across(7:10, ~sample(1:50, n())))

write_csv(example_ranking, 'data/example_ranking.csv')
