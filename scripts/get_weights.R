
## Create time weights

# Read weights data (based on days to draft) for Plackett-Luce model
time_weights_for_pl <- read_csv('data/weights_for_pl.csv')

# Calculate weights based on ranking date
time_weights <-
  ranking_dictionary %>%
  mutate(days_to_draft = as.numeric(ymd('2023-06-28') - ranking_date)) %>%
  left_join(time_weights_for_pl, by = 'days_to_draft') %>%
  pull(weight)

# Read historical publication rankings
publication_predictions <-
  map_dfr(2018:2022, ~read_csv(paste0('data/draft_', ., '.csv')) %>% mutate(draft_year = .x)) %>%
  clean_names() %>%
  select(-x1, -name) %>%
  select(draft_year, actual_pick, everything()) %>%
  pivot_longer(3:ncol(.), names_to = 'publication', values_to = 'prediction') %>%
  filter(prediction <= 30)

# Load pick value chart from Shuckers
pick_values <-
  read_table('data/pick_value_chart.txt') %>%
  select(value_1 = 2, value_2 = 3) %>%
  mutate(
    prediction = 1:30, 
    weight = ifelse(value_1 > 100, value_1, value_2),
    weight = weight/sum(weight)) %>%
  select(prediction, weight)

# Create model set to predict publication error for weights
model_set <-
  publication_predictions %>%
  left_join(pick_values, by = 'prediction') %>%
  mutate(error = abs(prediction - actual_pick)) %>%
  group_by(draft_year, publication) %>%
  summarize(metric = mean(error*weight)) %>%
  ungroup() %>%
  mutate(overall_metric = mean(metric)) %>%
  group_by(draft_year) %>%
  mutate(annual_metric = mean(metric)) %>%
  ungroup() %>%
  mutate(adj_metric = metric + overall_metric - annual_metric) %>%
  arrange(publication, draft_year) %>%
  group_by(publication) %>%
  mutate(other_metric = (sum(adj_metric) - adj_metric)/(n() - 1)) %>%
  ungroup() %>%
  drop_na()

# Train publication error outside year n on publication error in year n 
lm_model <- lm(adj_metric ~ other_metric, model_set)

# Assign publication weights
publication_weights_for_pl <-
  model_set %>%
  group_by(publication) %>%
  summarize(other_metric = mean(adj_metric)) %>%
  mutate(
    publication = gsub('_', '', publication),
    weight = predict(lm_model, .),
    weight = 1/weight,
    weight = n()*weight/sum(weight)) %>%
  select(publication, weight)

# Save publication name for each rankings
rankings_names <-
  read_csv('data/draft_2023.csv') %>%
  colnames() %>%
  gsub("\\..*","",.)

# Create final weights using time weights and publication weights
final_weights <-
  tibble(
    publication = tolower(gsub(' ', '', rankings_names))
  ) %>%
  mutate(
    publication = ifelse(publication == 'mckeen', 'mckeens', publication)
  ) %>%
  filter(nchar(publication) > 1) %>%
  left_join(publication_weights_for_pl, by = 'publication') %>%
  add_count(publication) %>%
  mutate(
    weight = replace_na(weight, 0.75), 
    time_weights = time_weights) %>%
  group_by(publication) %>%
  mutate(
    std_time_weights = (time_weights/sum(time_weights)),
    final_weights = weight*std_time_weights,
    final_weights = ifelse(n == 1, weight*time_weights, final_weights)) %>%
  pull(final_weights) %>%
  unname()

