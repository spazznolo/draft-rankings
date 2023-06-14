
library(ggpubr)

weighted_frequentist_probabilities <-
  read_csv('data/weighted_frequentist_probabilities.csv')

naive_bayesian_probabilities <-
  read_csv('data/naive_bayesian_probabilities.csv')

weighted_bayesian_probabilities <-
  read_csv('data/weighted_bayesian_probabilities.csv')

player_example <- c('SANDIN PELLIKKA, AXEL')

rankings_over_time <-
  draft_rankings %>%
  filter(Skater %in% player_example) %>%
  inner_join(ranking_dictionary, by = 'ranking_name') %>%
  group_by(Skater, ranking_date) %>%
  summarize(rank = -mean(rank)) %>%
  ggplot() +
  geom_point(aes(ranking_date, rank)) +
  geom_smooth(aes(ranking_date, rank))

probabilities_by_method <-
  bind_rows(weighted_frequentist_probabilities, naive_bayesian_probabilities, weighted_bayesian_probabilities) %>%
  filter(Skater %in% player_example, rank < 64) %>%
  group_by(Skater, method) %>%
  mutate(probability = cumsum(probability)) %>%
  ungroup() %>%
  ggplot() +
  geom_vline(xintercept = c(10, 20), alpha = 0.25, linetype = 'dashed') +
  geom_line(aes(rank, probability, col = method)) +
  xlim(0, 30)

ggarrange(rankings_over_time, probabilities_by_method, ncol=2, nrow=1, labels = player_example)
