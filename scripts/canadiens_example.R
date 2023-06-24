
library(gt)
library(gtExtras)

weighted_frequentist_probabilities <- read_csv('data/weighted_frequentist_probabilities.csv')

canadiens_example <-
  weighted_frequentist_probabilities %>%
  filter(Skater %in% c('BEDARD, CONNOR', 'FANTILLI, ADAM', 'MICHKOV, MATVEI', 'CARLSSON, LEO', 'SMITH, WILLIAM'), 
         rank <= 5) %>%
  group_by(Skater) %>%
  mutate(probability = cumsum(probability)) %>%
  ungroup() %>%
  mutate(probability = 1 - probability) %>%
  mutate(pick = rank + 1) %>%
  pivot_wider(id_cols = pick, names_from = Skater, values_from = probability) %>%
  filter(pick < 6) %>%
  add_row(pick = 1, 'BEDARD, CONNOR' = 1, 'FANTILLI, ADAM' = 1, 'MICHKOV, MATVEI' = 1, 
          'CARLSSON, LEO' = 1, 'SMITH, WILLIAM' = 1) %>%
  arrange(pick) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_percent(columns = 2:6)

# gtsave(canadiens_example, 
#        "draft-probabilities-3-1.png", 
#        path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',
#        expand = 0)

(1.0000*24)
(0.002*24) + (0.998*19)
(0.000*24) + (0.210*19) + (0.790*17.5)
(0.000*24) + (0.028*19) + (0.420*17.5) + (0.552*14.0)
(0.000*24) + (0.002*19) + (0.111*17.5) + (0.269*14.0) + (0.618*12.0)

(0.000*24) + (0.210*19) + (0.790*17.5)



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

predicted_outcomes <-
  tibble(
    bedard = rnorm(10000, 24, 3),
    fantilli = rnorm(10000, 19, 4),
    michkov = rnorm(10000, 17.5, 7),
    carlsson = rnorm(10000, 14, 4.5),
    smith = rnorm(10000, 12, 5))

predicted_outcomes %>%
  pivot_longer(everything(), names_to = 'prospect', values_to = 'war') %>%
  mutate(war = ifelse(war < 0, 0, war)) %>%
  ggplot() +
  geom_density(aes(war, fill = prospect), alpha = 0.75) +
  scale_fill_manual(values = wes_palette("Zissou1", n = 5)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()  # Customize major grid lines to be black
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

library(ggridges)

predicted_outcomes %>%
  pivot_longer(everything(), names_to = 'prospect', values_to = 'war') %>%
  mutate(
    war = ifelse(war < 0, 0, war), 
    prospect = toupper(prospect),
    prospect = fct_reorder(prospect, -war)) %>%
  ggplot() +
  geom_density_ridges(aes(x = war, y = prospect, fill = prospect), alpha = 0.75) +
  scale_fill_manual(values = wes_palette("Zissou1", n = 5)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none'
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

ggsave()

# multiply db by vector of probabilitiies
pick_1 <-
  predicted_outcomes

