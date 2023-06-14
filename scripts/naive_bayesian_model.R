
## Build Stan model

# Get necessary variables for Stan model
skaters <- nrow(skater_dictionary)
rankings <- nrow(ranking_dictionary)
ranking_list_lengths <- rep(skaters, rankings) 
ranking_weights <- rep(1, rankings) 
unique_weights <- 1 
predicted_ranks <- 1:(skaters - 1)

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

# Reformat the data to work well with ggplot2
fdata <- foreach(i = predicted_ranks, .combine = rbind) %do% {
  out <- data.frame(Skater = skater_dictionary$Skater[predicted_ranks[i]], Rank = as.vector(draft_simulations[, i]))
  names(out)[2] <- "Rank"
  out
}

# Calculate the probabilities of being in the top five ranks
dd <- tabyl(fdata, Skater, Rank)
rtab <- dd
rtab[, 2:skaters] <- rtab[, 2:skaters] / (nrow(fdata) / (skaters - 1))

naive_bayesian_probabilities <-
  rtab %>%  # Rearrange the columns to have the skater name as the first column
  pivot_longer(2:skaters, names_to = 'rank', values_to = 'probability') %>%
  mutate(method = 'naive_bayesian', rank = parse_number(rank))  # Rename the columns to represent the ranks

# Write the draft probabilities to a CSV file
write_csv(naive_bayesian_probabilities, 'data/naive_bayesian_probabilities.csv')

