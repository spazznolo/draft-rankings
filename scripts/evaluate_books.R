
sportsbook_ou <-
  read_csv('~/Documents/projects/draft-rankings/data/over_under.txt') %>%
  mutate(
    over_prob = ifelse(over > 0, 100/(100 + over), -over/-(over - 100)),
    under_prob = ifelse(under > 0, 100/(100 + under), -under/-(under - 100)),
    take = ((over_prob + under_prob) - 1)/2,
    across(over_prob:under_prob, ~ . - take),
    rank = position - 0.5) %>%
  filter(date == '2023-06-28') %>%
  select(book, skater, rank, prediction = under_prob)

ou_probabilities <-
  weighted_frequentist_probabilities %>%
  group_by(Skater) %>%
  mutate(prediction = cumsum(probability)) %>%
  ungroup() %>%
  mutate(
    book = 'model',
    skater = sub("(\\w+),\\s(\\w+)","\\2 \\1", Skater)
    ) %>%
  mutate(skater = ifelse(skater == 'SANDIN AXEL PELLIKKA', 'AXEL SANDIN PELLIKKA', skater)) %>%
  mutate(skater = ifelse(skater == 'GABRIEL PERREAULT', 'GABE PERREAULT', skater)) %>%
  select(book, skater, rank, prediction) %>%
  inner_join(sportsbook_ou %>% distinct(skater, rank), by = c('skater', 'rank'))

ou <-
  bind_rows(ou_probabilities, sportsbook_ou) %>% 
  pivot_wider(id_cols = c(skater, rank), names_from = book, values_from = prediction) %>%
  arrange(rank) %>%
  mutate(outcome = c(0, 0, 0, 1, 0, 1, 0, 0, 0, 1,
                     0, 1, 1, 1, 0, 1, 0, 0, 0, 1,
                     0, 1, 0, 1, 1, 0, 0, 0, 1, 0,
                     1, 1, 1, 0))

ou %>%
  mutate(across(model:betano, ~ abs(. - outcome)^2)) %>%
  filter(rank > 10 & !is.na(model) & !is.na(bet365)) %>%
  summarize(across(model:betano, ~mean(.)))


sportsbook_h2h <-
  read_csv('~/Documents/projects/draft-rankings/data/head_to_head.txt') %>%
  mutate(
    prob_1_prob = ifelse(prob_1 > 0, 100/(100 + prob_1), -prob_1/-(prob_1 - 100)),
    prob_2_prob = ifelse(prob_2 > 0, 100/(100 + prob_2), -prob_2/-(prob_2 - 100)),
    take = ((prob_1_prob + prob_2_prob) - 1)/2,
    across(prob_1_prob:prob_2_prob, ~ . - take)) %>%
  filter(date == '2023-06-28') %>%
  select(book, skater_1, skater_2, prediction = prob_1_prob)

head_to_head_probabilities <- 
  read_csv('data/head_to_head_probabilities.csv') %>%
  mutate(
    across(skater_x:skater_y, ~sub("(\\w+),\\s(\\w+)","\\2 \\1", .)),
    across(skater_x:skater_y, ~case_when(
      . == 'DMITRIY SIMASHEV' ~ 'DMITRI SIMASHEV',
      . == 'DANIL BUT' ~ 'DANIIL BUT',
      . == 'GABRIEL PERREAULT' ~ 'GABE PERREAULT',
      TRUE ~ .)),
    book = 'model') %>%
  select(book, skater_1 = skater_x, skater_2 = skater_y, prediction = comp) %>%
  inner_join(sportsbook_h2h %>% select(skater_1, skater_2) %>% distinct(), by = c('skater_1', 'skater_2'))

head_to_head <-
  bind_rows(head_to_head_probabilities, sportsbook_h2h) %>%
  pivot_wider(id_cols = c(skater_1, skater_2), names_from = book, values_from = prediction) %>%
  mutate(outcome = c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 
                     0, 1, 1, 1, NA, 0, 1))

head_to_head %>%
  mutate(across(model:betano, ~ abs(. - outcome)^2)) %>%
  filter(!is.na(model) & !is.na(betano)) %>%
  summarize(across(model:betano, ~mean(.)))


sportsbook_ep <-
  read_csv('~/Documents/projects/draft-rankings/data/exact_pick.txt') %>%
  mutate(prob = ifelse(odds > 0, 100/(100 + odds), -odds/-(odds - 100))) %>%
  filter(date == '2023-06-28') %>%
  select(book, pick, skater, prediction = prob)

prop_pick_probabilities <-
  weighted_frequentist_probabilities %>%
  mutate(
    book = 'model',
    skater = sub("(\\w+),\\s(\\w+)","\\2 \\1", Skater),
    skater = ifelse(skater == 'WILLIAM SMITH', 'WILL SMITH', skater)
  ) %>%
  select(book, skater, pick = rank, prediction = probability) %>%
  inner_join(sportsbook_ep %>% select(skater, pick) %>% distinct(), by = c('skater', 'pick'))

exact_picks <-
  bind_rows(prop_pick_probabilities, sportsbook_ep) %>%
  pivot_wider(id_cols = c(skater, pick), names_from = book, values_from = prediction) %>%
  mutate(outcome = c(0, 1, 0,
                     0, 0, 0, 0,
                     1, 0, 0,
                     0, 1, 0,
                     0, 0, 0, 1))


outcomes <-
  bind_rows(exact_picks, head_to_head, ou) %>%
  select(model, bet365, draftkings, betano, outcome) %>%
  mutate(across(model:betano, ~ (. - outcome)^2))

outcomes %>%
  summarize(across(model:betano, ~mean(., na.rm = TRUE)))

outcomes %>%
  filter(!is.na(model) & !is.na(betano)) %>%
  summarize(across(model:betano, ~mean(.)))

outcomes %>%
  filter(!is.na(model) & !is.na(bet365)) %>%
  summarize(across(model:betano, ~mean(.)))

outcomes %>%
  filter(!is.na(model) & !is.na(draftkings)) %>%
  summarize(across(model:betano, ~mean(.)))




log_loss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

exact_picks %>%
  mutate(across(model:betano, ~ log_loss(., outcome))) %>%
  filter(!is.na(model) & !is.na(betano)) %>%
  summarize(across(model:betano, ~mean(.)))

exact_picks %>%
  mutate(across(model:betano, ~ (. - outcome)^2)) %>%
  filter(!is.na(model) & !is.na(betano)) %>%
  summarize(across(model:betano, ~mean(.)))

exact_picks %>%
  #mutate(across(model:betano, ~ (. - outcome)^2)) %>%
  filter(!is.na(model) & !is.na(bet365)) %>%
  summarize(across(model:betano, ~ log_loss(., outcome)))

exact_picks %>%
  #mutate(across(model:betano, ~ abs(. - outcome))) %>%
  filter(!is.na(model) & !is.na(draftkings) & !is.na(betano) & !is.na(bet365)) %>%
  summarize(across(model:betano, ~ log_loss(., outcome)))




