
## Create time weights

# Read weights data (based on days to draft) for Plackett-Luce model
time_weights_for_pl <- read_csv('data/weights_for_pl.csv')

# Calculate weights based on ranking date
time_weights <-
  ranking_dictionary %>%
  mutate(days_to_draft = as.numeric(ymd('2023-06-28') - ranking_date)) %>%
  left_join(time_weights_for_pl, by = 'days_to_draft') %>%
  pull(weight)
