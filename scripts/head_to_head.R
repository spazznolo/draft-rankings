

reformat_simulations <- function(df_, skater_id_) {
  
  apply(df_, 2, function(x) which(x == skater_id_))
  
}

test <- map_dfc(1:skaters, ~reformat_simulations(draft_simulations, .))

head_to_head <- 
  expand_grid(x = 1:skaters, y = 1:skaters) %>%
  filter(x != y) %>%
  mutate(comp = map2_vec(x, y, ~round(mean(test[,.x] < test[,.y]), 3))) %>%
  inner_join(skater_dictionary %>% rename(skater_x = Skater), by = c('x' = 'skater_id')) %>%
  inner_join(skater_dictionary %>% rename(skater_y = Skater), by = c('y' = 'skater_id')) %>%
  select(-x, -y)

write_csv(head_to_head, 'data/head_to_head_probabilities.csv')
