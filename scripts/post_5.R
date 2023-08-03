
# step 1: simulate up to wings first pick (assume pick 9)
# step 2: remove 8 chosen prospects from analysis
# step 3: sample pick from remaining prospects ranked from 9:20
# step 4: save pick probabilities for pick 17
# step 5: multiply probabilities in step 4 by sampled prospect value in step 3
# step 6: save values
# step 7: repeat steps 3:6 for each prospect ranked 9:20



get_remaining_probs <- function(x_vec) {
  
  n = rep(0, length(x_vec))
  
  for (i in 1:length(x_vec)) {
    
    n[i] = x_vec[i]
    
    if (sum(n) >= 1) {
      n[i] = 1 - sum(n[1:(i-1)])
      return(n) 
    }
  }
  
}

gsva_value <- c(17.1, 12.3, 10.2, 9, 8.2, 7.6, 7.1, 6.7, 6.3, 6.0,
                   5.7, 5.5, 5.3, 5.1, 4.9, 4.7, 4.5, 4.3, 4.2, 4.0,
                   3.9, 3.7, 3.6, 3.5, 3.4, 3.2, 3.1, 3, 2.9, 2.8, 
                   2.7, 2.6, 2.5, 2.4, 2.4, 2.3, 2.2, 2.2, 2.1, 2.0, 
                   2.0, 1.9, 1.9, 1.8, 1.7, 1.7, 1.7, 1.6, 1.6, 1.5, 
                   1.5, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, 1.2, 
                   1.1, 1.1, 1.1, 1.1)

weighted_frequentist_probabilities <- read_csv('data/weighted_frequentist_probabilities.csv')

pick_values <- tibble(
  ranker = numeric(), skater = character(), pick_9 = numeric(), pick_17 = numeric(), total = numeric())

for (p in c(65, 90, 91)) {
  
  example_rankings <- unname(full_ranking_matrix[p,])
  
  first_picks <- 
    skater_dictionary %>%
    filter(Skater %in% c('BEDARD, CONNOR', 'CARLSSON, LEO', 'FANTILLI, ADAM', 'SMITH, WILLIAM', 'REINBACHER, DAVID',
                         'SIMASHEV, DMITRIY', 'MICHKOV, MATVEI', 'LEONARD, RYAN')) %>%
    pull(skater_id)
  
  all_available_picks <- example_rankings[-which(example_rankings %in% first_picks)]
  
  skater_value <- 
    tibble(
      skater_id = example_rankings, 
      skater_value = c(gsva_value, rep(0, (210 - length(gsva_value))))) %>%
    filter(!(skater_id %in% first_picks))
  
  pick_value = c()
  
  for (i in 1:2) {

    live_picks_new <- c(first_picks, all_available_picks[i])
    
    skater_list <- 1:skaters
    live_skaters <- skater_list[-live_picks_new]
    skaters_left <- length(live_picks_new)
    live_mle_estimates <- unname(mle_estimates[-live_picks_new])
    
    # Simulate draft rankings
    live_draft_simulations <- 
      replicate(10000, sample(live_skaters, 10, replace = FALSE, prob = live_mle_estimates)) %>%  # Simulate 100,000 drafts based on the estimated probabilities
      matrix(., nrow = 10, ncol = 10000)  # Convert the simulated drafts to a matrix format
    
    # Calculate probabilities for each rank for each skater
    list_of_probabilities <- map(live_skaters, ~rowSums(live_draft_simulations == .) / 10000)
    
    # Create a data frame with draft probabilities
    pick_contributions <-
      do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
      as_tibble() %>%  # Convert the matrix to a tibble
      mutate(skater_id = live_skaters) %>%
      filter(skater_id %in% all_available_picks) %>%
      pivot_longer(cols = V1:V9) %>%
      group_by(skater_id) %>%
      mutate(value = 1 - cumsum(value)) %>%
      ungroup() %>%
      filter(name == 'V8') %>%
      select(skater_id, prob_remaining = value) %>%
      left_join(skater_value, by = 'skater_id') %>%
      arrange(desc(skater_value)) %>%
      mutate(pick_contribution = get_remaining_probs(prob_remaining))
    print(pick_contributions$pick_contribution)
    value_17 <-
      pick_contributions %>%
      summarize(value = sum(skater_value*pick_contribution)) %>%
      pull(value)
    
    value_9 <- skater_value %>% filter(skater_id == all_available_picks[i]) %>% pull(skater_value)
    pick_value[i] = value_17 + value_9
    
    skater_name <- 
      skater_dictionary %>% 
      filter(skater_id == all_available_picks[i]) %>%
      pull(Skater)
    
    pick_values <- 
      pick_values %>%
      add_row(ranker = p, skater = skater_name, pick_9 = value_9, pick_17 = value_17, total = value_17 + value_9)
    
  }
  
}

first_picks <- 
  skater_dictionary %>%
  filter(Skater %in% c('BEDARD, CONNOR', 'CARLSSON, LEO', 'FANTILLI, ADAM', 'SMITH, WILLIAM', 'REINBACHER, DAVID',
                       'SIMASHEV, DMITRIY', 'MICHKOV, MATVEI', 'LEONARD, RYAN')) %>%
  pull(Skater)


peters_board <- 
  tibble(
    peters_skater = skater_dictionary[unname(full_ranking_matrix[65,]),]$Skater,
    peters_rank = 1:210,
    peters_value = c(gsva_value, rep(0, (210 - length(gsva_value))))) %>%
  filter(!(peters_skater %in% first_picks))

mckenzie_board <- 
  tibble(
    mckenzie_skater = skater_dictionary[unname(full_ranking_matrix[90,]),]$Skater,
    mckenzie_rank = 1:210,
    mckenzie_value = c(gsva_value, rep(0, (210 - length(gsva_value))))) %>%
  filter(!(mckenzie_skater %in% first_picks))

robinson_board <- 
  tibble(
    robinson_skater = skater_dictionary[unname(full_ranking_matrix[91,]),]$Skater,
    robinson_rank = 1:210,
    robinson_value = c(gsva_value, rep(0, (210 - length(gsva_value))))) %>%
  filter(!(robinson_skater %in% first_picks))

analyst_rankings <-
  bind_cols(peters_board, mckenzie_board, robinson_board) %>%
  mutate(new_rank = 1:n()) %>%
  slice(1:10) %>%
  select(new_rank, everything())

analyst_example <-
  analyst_rankings %>%
  gt() %>%
  tab_spanner(
    label = "PETERS",
    columns = c(peters_skater:peters_value)) %>%
  tab_spanner(
    label = "MCKENZIE",
    columns = c(mckenzie_skater:mckenzie_value)) %>%
  tab_spanner(
    label = "ROBINSON",
    columns = c(robinson_skater:robinson_value)) %>%
  cols_label(
    new_rank ~ "Rank at 9",
    peters_skater = "Skater",
    peters_rank = "Rank",
    peters_value = "Value",
    mckenzie_skater = "Skater",
    mckenzie_rank = "Rank",
    mckenzie_value = "Value",
    robinson_skater = "Skater",
    robinson_rank = "Rank",
    robinson_value = "Value"
  ) %>%
  tab_options(
    heading.background.color = 'black',
    column_labels.background.color = 'black',
    table.background.color = 'black'  # Set the table background color to black
  ) %>%
  gt_theme_nytimes()

analyst_example

gtsave(analyst_example,
       "draft-probabilities-5-4.png",
       path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',
       expand = 0)


wings_example <-
  pick_values %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  group_split(ranker, .keep = FALSE) %>%
  bind_cols() %>%
  clean_names() %>%
  mutate(new_rank = 1:n()) %>%
  select(new_rank, everything()) %>%
  gt() %>%
  tab_spanner(
    label = "PETERS",
    columns = c(skater_1:total_4)) %>%
  tab_spanner(
    label = "MCKENZIE",
    columns = c(skater_5:total_8)) %>%
  tab_spanner(
    label = "ROBINSON",
    columns = c(skater_9:total_12)) %>%
  cols_label(
    new_rank ~ "Rank at 9",
    skater_1 = "Skater",
    pick_9_2 = "Pick 9",
    pick_17_3 = "Pick 17",
    total_4 = "Total",
    skater_5 = "Skater",
    pick_9_6 = "Pick 9",
    pick_17_7 = "Pick 17",
    total_8 = "Total",
    skater_9 = "Skater",
    pick_9_10 = "Pick 9",
    pick_17_11 = "Pick 17",
    total_12 = "Total"
  ) %>%
  tab_options(
    heading.background.color = 'black',
    column_labels.background.color = 'black',
    table.background.color = 'black'  # Set the table background color to black
  )


gtsave(wings_example,
       "draft-probabilities-5-5.png",
       path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',
       expand = 0)

