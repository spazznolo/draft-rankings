
## Build Plackett-Luce model

# Reformat ranking matrix for PlackettLuce function
reformated_ranking_matrix <- map_dfc(1:skaters, ~reformat_ranking_matrix(full_ranking_matrix, .))

# Fit the Plackett-Luce model
pl_model <- PlackettLuce(reformated_ranking_matrix, weights = time_weights, maxit = 1000)

# Obtain maximum likelihood estimates from the Plackett-Luce model
mle_estimates <- coef(pl_model, log = FALSE)

# Assign pre-draft prospect rankings based on mle estimates
skater_dictionary$pre_draft_rank <- rank(-unname(mle_estimates))


## Derive draft probabilities

# Simulation count
n_simulations <- 5000

# Simulate draft rankings
draft_simulations <- 
  replicate(n_simulations, sample(1:skaters, skaters, replace = FALSE, prob = mle_estimates)) %>%  # Simulate drafts based on the estimated probabilities
  matrix(., nrow = skaters, ncol = n_simulations)  # Convert the simulated drafts to a matrix format

# Calculate probabilities for each rank for each skater
list_of_probabilities <- map(1:skaters, ~rowSums(draft_simulations == .) / n_simulations)

# Create a data frame with draft probabilities
weighted_frequentist_probabilities <-
  do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  mutate(Skater = skater_dictionary$Skater) %>%  # Add the skater names from the skater dictionary
  select(Skater, everything()) %>%  # Rearrange the columns to have the skater name as the first column
  pivot_longer(2:(skaters + 1), names_to = 'rank', values_to = 'probability') %>%
  mutate(method = 'weighted_frequentist', rank = parse_number(rank))  # Rename the columns to represent the ranks


