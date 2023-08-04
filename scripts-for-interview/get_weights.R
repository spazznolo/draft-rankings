
## Load time weights. 
## These were taken from an old project I did regarding user mock drafts.
## User mock drafts were used to estimate prospect pick probabilities.
## There, I looked at the effect of ranking date on ranking error.
## LOESS was used to smooth out the effect and obtain weights.

# Read weights data (based on days to draft) for Plackett-Luce model
time_weights_for_pl <- read_csv('data/weights_for_pl.csv')

# Calculate weights based on ranking date
time_weights <-
  ranking_dictionary %>%
  mutate(days_to_draft = as.numeric(ymd('2023-06-28') - ranking_date)) %>%
  left_join(time_weights_for_pl, by = 'days_to_draft') %>%
  pull(weight)

plot(ranking_dictionary$ranking_date, time_weights)
