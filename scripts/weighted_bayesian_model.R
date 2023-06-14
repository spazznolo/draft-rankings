# This code performs simulations and calculations related to draft rankings.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(cmdstanr)  # For using CmdStan for simulations
library(PLMIX)
library(magrittr)
library(janitor)  # For data cleaning and tabulation
library(foreach)  # For iterating over elements

select <- dplyr::select

# Introduction:
# The goal of this code is to simulate draft rankings and calculate probabilities for each skater's rank.
# It takes draft rankings data, pre-processes it, creates a ranking matrix, performs simulations, and calculates probabilities.


## Load and format data

# Read and preprocess draft rankings data
draft_rankings <- 
  read_csv('data/draft_rankings_4.csv') %>%  # Read draft rankings data from a CSV file
  pivot_longer(7:ncol(.), names_to = 'ranking_name', values_to = 'rank') %>%  # Convert wide format to long format
  drop_na() %>%  # Drop rows with missing values
  arrange(ranking_name, rank) %>%  # Sort the data by ranking date and rank
  filter(rank <= 100) %>%  # Keep only the top 100 ranks
  group_by(ranking_name) %>%  # Group the data by ranking date
  mutate(rank = 1:n()) %>%  # Create a rank column based on the row number within each group
  ungroup() %>%  # Ungroup the data
  add_count(ranking_name, name = 'ranking_size') %>%  # Add a column with the number of skaters in each ranking date
  mutate(rank_pct = rank / ranking_size) %>%  # Calculate the rank percentile within each ranking date
  group_by(Skater) %>%  # Group the data by skater
  mutate(best_rank = min(rank_pct)) %>%  # Determine the best rank achieved by each skater
  ungroup() %>%  # Ungroup the data
  filter(best_rank != 1)  # Exclude skaters who achieved the best rank

# Create a skater dictionary
skater_dictionary <-
  draft_rankings %>%  # Use the draft rankings data
  group_by(Skater) %>%
  summarize(mean_rank = mean(rank)) %>%
  arrange(mean_rank) %>%
  mutate(skater_id = 1:length(Skater)) %>%  # Assign a unique identifier to each skater 
  select(Skater, skater_id)

# Create a ranking dictionary
ranking_dictionary <-
  draft_rankings %>%
  count(ranking_name, name = 'ranking_size') %>%  # Count the number of skaters in each ranking
  mutate(
    ranking_id = 1:n(),  # Assign a unique identifier to each ranking
    ranking_date = sub("[.].*", "\\1", ranking_name),  # Extract the date from the ranking name
    ranking_date = mdy(ranking_date),  # Convert the date to the "month-day-year" format
    ranking_weight = case_when(  # Assign a weight based on the ranking date
      #ranking_date < '2022-08-31' ~ 4,
      ranking_date < '2023-01-05' ~ 3,
      ranking_date < '2023-04-30' ~ 2,
      TRUE ~ 1
    )
  )

# Create a partial ranking matrix
part_ranking_matrix <- 
  draft_rankings %>%  # Use the draft rankings data
  inner_join(skater_dictionary, by = 'Skater') %>%  # Join the draft rankings data with the skater dictionary
  pivot_wider(id_cols = ranking_name, names_from = rank, values_from = skater_id, values_fill = 0) %>%  # Convert the data to a wide format, with skater IDs as values
  select(2:ncol(.)) %>%  # Select only the columns representing the ranks (columns 2 to 101)
  as.matrix()  # Convert the data to a matrix

# Add empty columns to force ncols = skaters
ranking_matrix <- cbind(part_ranking_matrix, matrix(0, nrow = nrow(ranking_dictionary), ncol = (nrow(skater_dictionary) - ncol(part_ranking_matrix))))  # Combine the partial ranking matrix with an empty matrix for unranked skaters


## Create full rankings

# Get skater appearance counts
top_skater_freq <- 
  rank_summaries(
    data=ranking_matrix, 
    format_input="ordering", 
    mean_rank=TRUE,
    pc=FALSE) %>%
  .$marginals %>%
  colSums()

# Impute partial rankings to create full ranking matrix
full_ranking_matrix <- 
  make_complete(
    data=ranking_matrix, 
    format_input="ordering", 
    probitems=top_skater_freq) %>%
  .$completedata


# Build Stan model

# Get necessary variables for Stan model
skaters <- nrow(skater_dictionary)
rankings <- nrow(ranking_dictionary)
ranking_list_lengths <- rep(skaters, rankings) #ranking_dictionary$ranking_size
ranking_weights <- ranking_dictionary$ranking_weight
unique_weights <- n_distinct(ranking_dictionary$ranking_weight)
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

# Extract posterior ranks
#chc2 <- fit$summary("posterior_latent_ranks")

# Create a dataframe with posterior rank estimates
# post_data <- data.frame(
#   Name = skater_dictionary$Skater,
#   rank = chc2$mean,
#   Rl = chc2$q5,
#   Ru = chc2$q95
# )

# Subset the posterior data for the final ranks
#fdata <- post_data


## Get estimates

# Get the finals data from MCMC draws
finals_draws <- fit$draws("replay_ranking", format = "df")

# Reformat the data to work well with ggplot2
fdata <- foreach(i = predicted_ranks, .combine = rbind) %do% {
  out <- data.frame(Skater = skater_dictionary$Skater[predicted_ranks[i]], Rank = as.vector(finals_draws[, i]))
  names(out)[2] <- "Rank"
  out
}

# Calculate the probabilities of being in the top five ranks
dd <- tabyl(fdata, Skater, Rank)
rtab <- dd
rtab[, 2:skaters] <- rtab[, 2:skaters] / (nrow(fdata) / (skaters - 1))

weighted_bayesian_probabilities <-
  rtab %>%  # Rearrange the columns to have the skater name as the first column
  pivot_longer(2:(skaters), names_to = 'rank', values_to = 'probability') %>%
  mutate(method = 'weighted_bayesian', rank = parse_number(rank))  # Rename the columns to represent the ranks

# Save draft pick probabilities
write_csv(weighted_bayesian_probabilities, 'data/weighted_bayesian_probabilities.csv')






list_sims <- map(1:188, ~colSums(finals_draws == .) / 4000)  # Calculate the probabilities for each rank for each skater
hm <- 
  do.call(rbind, unname(list_sims)) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%
  select(1:188) %>%
  set_colnames(paste0('player_', 1:188))

hm %>%
  glimpse()
