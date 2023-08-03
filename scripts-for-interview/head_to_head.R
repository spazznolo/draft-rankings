
# Reformat the simulations to fit into a head-to-head framework
reformated_simulations <- map_dfc(1:skaters, ~reformat_simulations(draft_simulations, .))

# Derive head-to-head probabilities for each pair of skaters
head_to_head_probabilities <- 
  # Generate all possible pairs of skaters using 'expand_grid' and filter out cases where x equals y (same skater)
  expand_grid(x = 1:skaters, y = 1:skaters) %>%
  filter(x != y) %>%
  # Calculate the probability of skater x outperforming skater y in the simulations
  mutate(comp = map2_vec(x, y, ~round(mean(reformated_simulations[,.x] < reformated_simulations[,.y]), 3))) %>%
  # Join with 'skater_dictionary' to get the skater names and pre-draft ranks for both skaters (x and y)
  inner_join(skater_dictionary %>% rename(skater_x = Skater, pre_draft_rank_x = pre_draft_rank), by = c('x' = 'skater_id')) %>%
  inner_join(skater_dictionary %>% rename(skater_y = Skater, pre_draft_rank_y = pre_draft_rank), by = c('y' = 'skater_id')) %>%
  # Select only relevant columns
  select(skater_x, pre_draft_rank_x, skater_y, pre_draft_rank_y, comp)
