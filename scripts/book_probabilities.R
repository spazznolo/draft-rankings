
sportsbook_ou <-
  read_csv('~/Documents/projects/draft-rankings/data/over_under.txt') %>%
  mutate(
    over_prob = ifelse(over > 0, 100/(100 + over), -over/-(over - 100)),
    under_prob = ifelse(under > 0, 100/(100 + under), -under/-(under - 100)),
    take = ((over_prob + under_prob) - 1)/2,
    across(over_prob:under_prob, ~ . - take),
    rank = position - 0.5) %>%
  filter(date == '2023-06-22') %>%
  select(book, skater, rank, prediction = under_prob)
100/350
cml_probabilities <-
  weighted_frequentist_probabilities %>%
  group_by(Skater) %>%
  mutate(prediction = cumsum(probability)) %>%
  ungroup() %>%
  mutate(
    book = 'model',
    skater = sub("(\\w+),\\s(\\w+)","\\2 \\1", Skater)
    ) %>%
  select(book, skater, rank, prediction)

# cml_probabilities %>%
#   inner_join(sportsbook_ou %>% distinct(skater, rank), by = c('skater', 'rank')) %>%
#   bind_rows(., sportsbook_ou) %>%
#   unite('skater_rank', c(skater, rank)) %>%
#   distinct(skater_rank) %>%
#   mutate(outcome = c())

# Function to create a dark theme for plots
dark_theme <- function() {
  
  theme(
    panel.grid.major = element_line(color = '#99a1b3'),  # Customize major grid lines
    panel.grid.minor = element_line(color = 'black'),  # Customize minor grid lines
    panel.background = element_rect(fill = 'black'),  # Set panel background color
    panel.border = element_blank(),  # Remove panel borders
    plot.background = element_rect(fill = "black", color = "black"),  # Set plot background color
    axis.text.x = element_text(color = 'white'),  # Customize x-axis tick labels
    axis.text.y = element_text(color = 'white'),  # Customize y-axis tick labels
    plot.title = element_text(face = 'bold', color = 'white', hjust = 0.5),  # Customize plot title
    plot.subtitle = element_text(face = 'bold', color = 'white'),  # Customize plot subtitle
    axis.title.x = element_text(color = 'white'),  # Customize x-axis title
    axis.title.y = element_text(color = 'white'),  # Customize y-axis title
    legend.background = element_rect(fill = "black", color = NA),  # Set legend background color
    legend.key = element_rect(color = "gray", fill = "black"),  # Customize legend key
    legend.title = element_text(color = "white"),  # Customize legend title
    legend.text = element_text(color = "white")  # Customize legend text
  )
  
}

library(wesanderson)

cml_probabilities %>%
  mutate(skater = ifelse(skater == 'SANDIN AXEL PELLIKKA', 'AXEL SANDIN PELLIKKA', skater)) %>%
  mutate(skater = ifelse(skater == 'GABRIEL PERREAULT', 'GABE PERREAULT', skater)) %>%
  inner_join(sportsbook_ou %>% distinct(skater, rank), by = c('skater', 'rank')) %>%
  bind_rows(., sportsbook_ou) %>%
  unite('skater_rank', c(skater, rank)) %>%
  ggplot() +
  geom_point(aes(skater_rank, prediction, col = book)) +
  labs(x = 'Skater _ (picked by)', y = '\nImplied Probability') +
  scale_color_manual(values= wes_palette("Zissou1", n = 5)[c(1:3, 5)]) +
  scale_y_continuous(labels = scales::percent) +
  dark_theme() +  # Apply dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  coord_flip()

cml_probabilities %>%
  mutate(skater = ifelse(skater == 'SANDIN AXEL PELLIKKA', 'AXEL SANDIN PELLIKKA', skater)) %>%
  mutate(skater = ifelse(skater == 'GABRIEL PERREAULT', 'GABE PERREAULT', skater)) %>%
  inner_join(sportsbook_ou %>% distinct(skater, rank), by = c('skater', 'rank')) %>%
  bind_rows(., sportsbook_ou) %>%
  unite('skater_rank', c(skater, rank)) %>%
  pivot_wider(id_cols = skater_rank, names_from = book, values_from = prediction) %>%
  mutate(diff = abs(model - bet365)) %>%
  arrange(desc(diff))

