

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


split(draf)

# fix dates
draft_rankings %>%
  distinct(ranking_date) %>%
  pull(ranking_date) %>%
  str_split(., '[.]')
  mutate(ranking_date = gsub('[.]+[1-9]', '', ranking_date))

library(lubridate)
  
draft_rankings %>%
  mutate(ranking_date = str_split(ranking_date, '[.]')[[1]]) %>%
  mutate(ranking_date = mdy(ranking_date))

ranking_dictionary <-
  draft_rankings %>%
  distinct(ranking_date) %>%
  mutate(ranking_id = 1:n())

draft_rankings %>%
  group_by(ranking_date) %>%
  mutate(group_id = cur_group_id()) %>%
  distinct(ranking_date, group_id)
?complete




data(d_dublinwest)
head(d_dublinwest)
top_item_freq <- rank_summaries(data=d_dublinwest, format_input="ordering", mean_rank=TRUE,
                                pc=FALSE)$marginals["Rank_1",]
d_dublinwest_compl <- make_complete(data=d_dublinwest, format_input="ordering",
                                    probitems=top_item_freq)
head(d_dublinwest_compl$completedata)



sd <- 
  draft_rankings %>%
  count(ranking_date, sort = TRUE) %>%
  summarize(quantile(n))
