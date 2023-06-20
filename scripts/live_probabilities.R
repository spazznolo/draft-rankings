

live_picks <- c('BEDARD, CONNOR', 'FANTILLI, ADAM')
live_picks <- skater_dictionary$Skater[1:10]
prospects_taken <- which(skater_dictionary$Skater %in% live_picks)

skater_list <- 1:skaters
live_skaters <- skater_list[-prospects_taken]
skaters_left <- length(live_skaters)
live_mle_estimates <- mle_estimates[-prospects_taken]

# Simulate draft rankings
live_draft_simulations <- 
  replicate(100000, sample(live_skaters, skaters_left, replace = FALSE, prob = live_mle_estimates)) %>%  # Simulate 100,000 drafts based on the estimated probabilities
  matrix(., nrow = skaters_left, ncol = 100000)  # Convert the simulated drafts to a matrix format

# Calculate probabilities for each rank for each skater
list_of_probabilities <- map(live_skaters, ~rowSums(live_draft_simulations == .) / 100000)

new_skater_dictionary <-
  skater_dictionary %>%
  filter(!(skater_id %in% prospects_taken)) %>%
  mutate(skater_id = paste0('V', 1:n()))

# Create a data frame with draft probabilities
remaining_frequentist_probabilities <-
  do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  slice(1) %>%
  select(1:10) %>%
  pivot_longer(everything(), names_to = 'skater_id', values_to = 'prob_next') %>%
  left_join(new_skater_dictionary, by = 'skater_id')

paste0(
  "Pick ", length(live_picks), ":", live_picks[length(live_picks)],
  "Probability drafted this high:", probs)


paste0(
  "Prospect Probabilities for Pick ", length(live_picks)
)





