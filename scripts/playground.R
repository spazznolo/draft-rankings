

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

  gsub('(.*)_\\w+', '\\1', draft_rankings %>%
         distinct(ranking_date) %>%
         pull(ranking_date))
  
  sub("[.].*", "\\1", draft_rankings %>%
        distinct(ranking_name) %>%
        pull(ranking_name))

  ranking_dictionary <-
  draft_rankings %>%
  count(ranking_date, name = 'ranking_size') %>%
  mutate(ranking_date = str_split(ranking_date, '[.]')[[1]]) %>%
  mutate(ranking_date = mdy(ranking_date)) %>%
  mutate(ranking_id = 1:n())

str_split(draft_rankings$ranking_date, '_')[1]

sub("[...]*", '', draft_rankings$ranking_date %>% distinct())

ranking_dictionary <-
  draft_rankings %>%
  count(ranking_name, name = 'ranking_size') %>%
  mutate(
    ranking_id = 1:n(),
    ranking_date = sub("[.].*", "\\1", ranking_name),
    ranking_date = mdy(ranking_date),
    ranking_weight = case_when(
      ranking_date < '2022-08-31' ~ 1, 
      ranking_date < '2023-01-05' ~ 2, 
      ranking_date < '2023-04-30' ~ 3, 
      TRUE ~ 4))

ranking_dictionary %>% count(ranking_weight)

library(janitor)
# Read and preprocess draft rankings data
draft_rankings <- 
  read_csv('data/draft_rankings_2.csv') %>%  # Read draft rankings data from a CSV file
  clean_names() %>% 
  pivot_longer(7:ncol(.), names_to = 'ranking_date', values_to = 'rank') %>%  # Convert wide format to long format
  drop_na() %>%  # Drop rows with missing values
  arrange(ranking_date, rank) %>%  # Sort the data by ranking date and rank
  filter(rank <= 100) %>%  # Keep only the top 100 ranks
  group_by(ranking_date) %>%  # Group the data by ranking date
  mutate(rank = 1:n()) %>%  # Create a rank column based on the row number within each group
  ungroup() %>%  # Ungroup the data
  add_count(ranking_date, name = 'ranking_size') %>%  # Add a column with the number of skaters in each ranking date
  mutate(rank_pct = rank / ranking_size) %>%  # Calculate the rank percentile within each ranking date
  group_by(skater) %>%  # Group the data by skater
  mutate(best_rank = min(rank_pct)) %>%  # Determine the best rank achieved by each skater
  ungroup() %>%  # Ungroup the data
  filter(best_rank != 1) %>%  # Exclude skaters who only achieved the worst rank
  select(skater, ranking_date, rank)

?make_complete
PLMIX::make_complete()


data(d_dublinwest)
head(d_dublinwest)
sum(ranking_matrix > 188)

ranking_matrix <- cbind(part_ranking_matrix, matrix(0, nrow = 69, ncol = 89))  # Combine the partial ranking matrix with an empty matrix for unranked skaters

top_item_freq <- 
  rank_summaries(
    data=ranking_matrix, 
    format_input="ordering", 
    mean_rank=TRUE,
    pc=FALSE) %>%
  .$marginals %>%
  colSums()

full_ranking_matrix <- 
  make_complete(
    data=ranking_matrix, 
    format_input="ordering", 
    probitems=top_item_freq) %>%
  .$completedata

head(d_dublinwest_compl$completedata)


