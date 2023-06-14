
# This code performs simulations and calculations related to draft rankings.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(PLMIX)  # For performing probabilistic modeling

select <- dplyr::select

# Introduction:
# The goal of this code is to simulate draft rankings and calculate probabilities for each skater's rank.
# It takes draft rankings data, pre-processes it, creates a ranking matrix, performs simulations, and calculates probabilities.


# Read and preprocess draft rankings data
draft_rankings <- 
  read_csv('data/draft_rankings_2.csv') %>%  # Read draft rankings data from a CSV file
  pivot_longer(7:ncol(.), names_to = 'ranking_date', values_to = 'rank') %>%  # Convert wide format to long format
  drop_na() %>%  # Drop rows with missing values
  arrange(ranking_date, rank) %>%  # Sort the data by ranking date and rank
  filter(rank <= 100) %>%  # Keep only the top 100 ranks
  group_by(ranking_date) %>%  # Group the data by ranking date
  mutate(rank = 1:n()) %>%  # Create a rank column based on the row number within each group
  ungroup() %>%  # Ungroup the data
  add_count(ranking_date, name = 'ranking_size') %>%  # Add a column with the number of skaters in each ranking date
  mutate(rank_pct = rank / ranking_size) %>%  # Calculate the rank percentile within each ranking date
  group_by(Skater) %>%  # Group the data by skater
  mutate(best_rank = min(rank_pct)) %>%  # Determine the best rank achieved by each skater
  ungroup() %>%  # Ungroup the data
  filter(best_rank != 1)  # Exclude skaters who achieved the best rank

# Create a skater dictionary
skater_dictionary <-
  draft_rankings %>%  # Use the draft rankings data
  distinct(Skater) %>%  # Select unique skater names
  mutate(skater_id = 1:length(Skater))  # Assign a unique identifier to each skater

# Create a partial ranking matrix
part_ranking_matrix <-
  draft_rankings %>%  # Use the draft rankings data
  inner_join(skater_dictionary, by = 'Skater') %>%  # Join the draft rankings data with the skater dictionary
  pivot_wider(id_cols = ranking_date, names_from = rank, values_from = skater_id, values_fill = 0) %>%  # Convert the data to a wide format, with skater IDs as values
  select(2:ncol(.)) %>%  # Select only the columns representing the ranks (columns 2 to 101)
  as.matrix()  # Convert the data to a matrix

# Create the full ranking matrix
ranking_matrix <- cbind(part_ranking_matrix, matrix(0, nrow = 68, ncol = 87))  # Combine the partial ranking matrix with an empty matrix for unranked skaters


## Plackett-Luce Model for partial rankings (without time as covariate)

# Perform probabilistic modeling and obtain maximum a posteriori (MAP) estimates
MAP <- mapPLMIX(pi_inv = ranking_matrix, K = ncol(ranking_matrix), G = 1, n_iter = 500*1, )  # Perform probabilistic modeling using the PLMIX package

map_estimates <- unname(MAP$P_map)  # Extract the MAP estimates for the probabilities


## Simulate rankings based on MLE estimates extracted above

# Set seed for reproducibility
set.seed(33)

# Simulate draft rankings
draft_simulations <- 
  replicate(100000, sample(1:187, 187, replace = FALSE, prob = map_estimates)) %>%  # Simulate 100,000 draft rankings based on the estimated probabilities
  matrix(., nrow = 187, ncol = 100000)  # Convert the simulated rankings to a matrix format

list_sims <- map(1:187, ~rowSums(draft_simulations == .) / 100000)  # Calculate the probabilities for each rank for each skater

# Create a data frame with draft probabilities
draft_probabilities <-
  do.call(rbind, list_sims) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  mutate(Skater = skater_dictionary$Skater) %>%  # Add the skater names from the skater dictionary
  select(Skater, everything()) %>%  # Rearrange the columns to have the skater name as the first column
  rename_at(2:188, ~paste0('rank_', parse_number(.)))  # Rename the columns to represent the ranks

# The resulting data frame 'draft_probabilities' contains the simulated probabilities for each skater's rank.


#write_csv(draft_probabilities, 'data/draft_probabilities.csv')


