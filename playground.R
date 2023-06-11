

draft_probabilities %>%
  inner_join(skater_dictionary %>% slice(100:105), by = 'Skater') %>%
  pivot_longer(2:188) %>%
  mutate(rank = parse_number(name)) %>%
  ggplot() +
  geom_line(aes(rank, value, col = Skater)) +
  scale_x_continuous(limits = c(0, 187))


map2_vec(6:10, 7:11, ~mean(apply(draft_simulations, 2, function(x) which(x == .x) < which(x == .y))))

head_to_head_simulations <- function(p1, p2) {

  if (output < 0.999 & p1) {
    output <- mean(apply(draft_simulations, 2, function(x) which(x == p1) < which(x == p2))) 
  }
  
  else {
    output = 1
  }
  
  output
  
}
  
head_to_head_probabilities <-
  expand_grid(player_a = 1:187, player_b = 1:187) %>%
  filter(player_a < player_b) %>%
  mutate(
    prob_ahead = map2_vec(player_a, player_b, head_to_head_simulations)
  )



tabulate_apply <- function(){
  apply(draft_simulations, 1, tabulate)
}


