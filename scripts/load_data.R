# This code performs simulations and calculations related to draft rankings.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(cmdstanr)  # For using CmdStan for simulations
library(PLMIX)
library(magrittr)
library(janitor)  # For data cleaning and tabulation
library(foreach)  # For iterating over elements
library(PlackettLuce)
library(wesanderson)
library(ggridges)
library(ggpubr)

select <- dplyr::select

# Introduction:
# The goal of this code is to simulate draft rankings and calculate probabilities for each skater's rank.
# It takes draft rankings data, pre-processes it, creates a ranking matrix, performs simulations, and calculates probabilities.


## Load and format data

# Read and preprocess draft rankings data
draft_rankings <- 
  read_csv('data/draft_2023.csv', skip = 1) %>%  # Read draft rankings data from a CSV file
  pivot_longer(7:ncol(.), names_to = 'ranking_name', values_to = 'rank') %>%  # Convert wide format to long format
  drop_na() %>%  # Drop rows with missing values
  arrange(rank) %>%  # Sort the data by ranking date and rank
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
  distinct(ranking_name) %>%  # Count the number of skaters in each ranking
  mutate(
    ranking_id = 1:n(),
    ranking_date = sub("[.].*", "\\1", ranking_name),  # Extract the date from the ranking name
    ranking_date = mdy(ranking_date),  # Convert the date to the "month-day-year" format
    ranking_weight = case_when(  # Assign a weight based on the ranking date
      ranking_date < '2022-08-31' ~ 4,
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
    mean_rank=FALSE,
    pc=FALSE) %>%
  .$marginals %>%
  colSums()


# Impute partial rankings to create full ranking matrix
full_ranking_matrix <- 
  make_complete(
    data=ranking_matrix, 
    format_input="ordering", 
    probitems=unname(top_skater_freq)) %>%
  .$completedata


# Get necessary variables for rank-ordered logit models
skaters <- nrow(skater_dictionary)
rankings <- nrow(ranking_dictionary)
ranking_list_lengths <- rep(skaters, rankings)
ranking_weights <- ranking_dictionary$ranking_weight
unique_weights <- n_distinct(ranking_dictionary$ranking_weight)
predicted_ranks <- 1:(skaters - 1)


