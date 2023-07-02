
#weighted_frequentist_probabilities <- read_csv('data/weighted_frequentist_probabilities.csv')

live_picks <- skater_dictionary$Skater[1:8]

for (i in 9:20) {
  
  live_picks_new <- c(live_picks, skater_dictionary$Skater[i])
  prospects_taken <- which(skater_dictionary$Skater %in% live_picks_new)
  
  probability_of_pick <-
    weighted_frequentist_probabilities %>%
    group_by(Skater) %>%
    mutate(prediction = cumsum(probability)) %>%
    ungroup() %>%
    filter(rank == length(live_picks_new), Skater == live_picks_new[length(live_picks_new)]) %>%
    pull(prediction)
  
  skater_list <- 1:skaters
  live_skaters <- skater_list[-prospects_taken]
  skaters_left <- length(live_picks_new)
  live_mle_estimates <- mle_estimates[-prospects_taken]
  
  # Simulate draft rankings
  live_draft_simulations <- 
    replicate(10000, sample(live_skaters, skaters_left, replace = FALSE, prob = live_mle_estimates)) %>%  # Simulate 100,000 drafts based on the estimated probabilities
    matrix(., nrow = skaters_left, ncol = 10000)  # Convert the simulated drafts to a matrix format
  
  # Calculate probabilities for each rank for each skater
  list_of_probabilities <- map(live_skaters, ~rowSums(live_draft_simulations == .) / 10000)
  
  new_skater_dictionary <-
    skater_dictionary %>%
    filter(!(skater_id %in% prospects_taken))
  
  # Create a data frame with draft probabilities
  remaining_frequentist_probabilities <-
    do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
    as_tibble() %>%  # Convert the matrix to a tibble
    bind_cols(new_skater_dictionary, .) %>%
    arrange(desc(V8)) %>%
    select(Skater, V8)
  
  print(live_picks_new)
  print(remaining_frequentist_probabilities)
  
  # save prospect value for pick 9
  # derive pick value for pick 17 (using probabilities in remaining_frequentist_probabilities)
  
}


dobber <-
  draft_rankings %>%
  filter(ranking_name == '6/22/2023')






