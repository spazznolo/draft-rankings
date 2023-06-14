

missing_ranks <-
  draft_rankings %>% 
  pivot_longer(7:ncol(.)) %>%
  drop_na() %>%
  arrange(name, value) %>%
  group_by(name) %>%
  mutate(missing = ifelse(value - lag(value) != 1, 1, 0)) %>%
  ungroup() %>%
  filter(missing == 1) %>%
  mutate(missing_rank = value - 1) %>%
  select(rank_list = name, missing_rank)

write_csv(missing_ranks, 'data/missing_ranks.csv')
