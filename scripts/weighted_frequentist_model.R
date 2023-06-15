
# Read weights data (based on days to draft) for Plackett-Luce model
weights_for_pl <- read_csv('data/weights_for_pl.csv')

# Calculate weights based on ranking date
weights <-
  ranking_dictionary %>%
  mutate(days_to_draft = as.numeric(ymd('2023-06-28') - ranking_date)) %>%
  left_join(weights_for_pl, by = 'days_to_draft') %>%
  pull(weight)

# Fit the Plackett-Luce model
pl_model <- PlackettLuce(full_ranking_matrix, weights = weights, npseudo = 0.5, maxit = c(5000, 100))

# Obtain maximum likelihood estimates from the Plackett-Luce model
mle_estimates <- coef(pl_model, log = FALSE)

# Simulate draft rankings
draft_simulations <- 
  replicate(100000, sample(1:skaters, skaters, replace = FALSE, prob = mle_estimates)) %>%  # Simulate 100,000 draft rankings based on the estimated probabilities
  matrix(., nrow = skaters, ncol = 100000)  # Convert the simulated rankings to a matrix format

# Calculate probabilities for each rank for each skater
list_of_probabilities <- map(1:skaters, ~rowSums(draft_simulations == .) / 100000)

# Create a data frame with draft probabilities
weighted_frequentist_probabilities <-
  do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  mutate(Skater = skater_dictionary$Skater) %>%  # Add the skater names from the skater dictionary
  select(Skater, everything()) %>%  # Rearrange the columns to have the skater name as the first column
  pivot_longer(2:(skaters + 1), names_to = 'rank', values_to = 'probability') %>%
  mutate(method = 'weighted_frequentist', rank = parse_number(rank))  # Rename the columns to represent the ranks

# Write the draft probabilities to a CSV file
write_csv(weighted_frequentist_probabilities, 'data/weighted_frequentist_probabilities.csv')
