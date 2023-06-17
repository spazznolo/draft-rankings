
## Build Stan model

# Prepare the data for the Stan model
stan_dt <- 
  list(
    N = rankings,
    P = skaters,
    x = apply(full_ranking_matrix, 2, as.integer),
    real_ranks = 1:skaters,
    nps = ranking_list_lengths,
    P1 = skaters - 1,
    type = ranking_weights,
    finals = predicted_ranks,
    n_types = unique_weights
  )

# Load the Stan model
mod <- cmdstan_model("scripts/plackett_luce_opt.stan")

# Perform variational inference
fit_variational <- mod$variational(data = stan_dt)

# Run the MCMC sampling
fit <- mod$sample(
  data = stan_dt, 
  seed = 33, 
  parallel_chains = 2, 
  iter_warmup = 500, 
  iter_sampling = 2000)


## Get estimates

# Get the finals data from MCMC draws
draft_simulations <- fit$draws("replay_ranking", format = "df")

# Calculate probabilities for each rank for each skater
list_of_probabilities <- map(predicted_ranks, ~colSums(draft_simulations == .)[predicted_ranks] / 8000)  # Calculate the probabilities for each rank for each skater

# Create a data frame with draft probabilities
weighted_bayesian_probabilities <- 
  do.call(cbind, unname(list_of_probabilities)) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  mutate(Skater = skater_dictionary$Skater[predicted_ranks]) %>% # Add the skater names from the skater dictionary
  pivot_longer(1:(skaters - 1), names_to = 'rank', values_to = 'probability') %>%
  mutate(method = 'weighted_bayesian', rank = parse_number(rank))  # Rename the columns to represent the ranks

# Write the draft probabilities to a CSV file
write_csv(weighted_bayesian_probabilities, 'data/weighted_bayesian_probabilities.csv')

