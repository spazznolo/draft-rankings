
# step 1: simulate up to wings first pick (assume pick 9)
# step 2: remove 8 chosen prospects from analysis
# step 3: sample pick from remaining prospects ranked from 9:20
# step 4: save pick probabilities for pick 17
# step 5: multiply probabilities in step 4 by sampled prospect value in step 3
# step 6: save values
# step 7: repeat steps 3:6 for each prospect ranked 9:20




predicted_prospect_values <-
  tibble(
    bedard = rnorm(100000, 24, 3),
    fantilli = rnorm(100000, 19, 4),
    michkov = rnorm(100000, 17.5, 7),
    carlsson = rnorm(100000, 14, 4.5),
    smith = rnorm(100000, 12, 5))

predicted_prospect_values %>%
  pivot_longer(everything(), names_to = 'prospect', values_to = 'war') %>%
  mutate(war = ifelse(war < 0, 0, war)) %>%
  ggplot() +
  geom_density(aes(war, fill = prospect), alpha = 0.75) +
  scale_fill_manual(values = c('lightblue', "#78B7C5", "#EBCC2A", "yellowgreen", "#F21A00")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()  # Customize major grid lines to be black
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")



predicted_prospect_values %>%
  ggplot() +
  geom_density(aes(bedard), alpha = 0.75, fill = single_color, bandwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none'
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'draft-probabilities-4-1.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5.5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320,  # Set the resolution of the plot
  bg = 'white'
)


predicted_prospect_values %>%
  pivot_longer(everything(), names_to = 'prospect', values_to = 'war') %>%
  mutate(
    war = ifelse(war < 0, 0, war), 
    prospect = toupper(prospect),
    prospect = fct_reorder(prospect, -war)) %>%
  ggplot() +
  geom_density_ridges(aes(x = war, y = prospect, fill = prospect), alpha = 0.75, bandwidth = 1) +
  scale_fill_manual(values = rev(multiple_colors(5))) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none'
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'draft-probabilities-4-2.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5.5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320,  # Set the resolution of the plot
  bg = 'white'
)

weighted_frequentist_probabilities <- read_csv('data/weighted_frequentist_probabilities.csv')

top_5_probs <-
  weighted_frequentist_probabilities %>%
  mutate(rank = rank + 1) %>%
  group_by(Skater) %>%
  mutate(
    probability = cumsum(probability),
    probability = 1 - probability) %>%
  ungroup() %>%
  pivot_wider(id_cols = Skater, names_from = rank, values_from = probability)


get_remaining_probs <- function(x) {
  
  x_vec <- top_5_probs %>% pull(x)
  
  n = rep(0, length(x_vec))
  
  for (i in 1:length(x_vec)) {
    
    n[i] = x_vec[i]
    
    if (sum(n) >= 1) {
      n[i] = 1 - sum(n[1:(i-1)])
      return(n*10000) 
      }
  }
  
  
  
}

tsx <- map(2:25, get_remaining_probs)

apply_rnorm <- function(x, y, z) {
  
  if (x == 0) { return(vector()) }
  if (x > 0) { return(rnorm(x, y, z)) }
  
}

error_weights <- c(17.1, 12.3, 10.2, 9, 8.2, 7.6, 7.1, 6.7, 6.3, 6.0,
                   5.7, 5.5, 5.3, 5.1, 4.9, 4.7, 4.5, 4.3, 4.2, 4.0,
                   3.9, 3.7, 3.6, 3.5, 3.4, 3.2, 3.1, 3, 2.9, 2.8, 
                   2.7, 2.6, 2.5, 2.4, 2.4, 2.3, 2.2, 2.2, 2.1, 2.0, 
                   2.0, 1.9, 1.9, 1.8, 1.7, 1.7, 1.7, 1.6, 1.6, 1.5, 
                   1.5, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, 1.2, 
                   1.1, 1.1, 1.1, 1.1)

new_pick_values <-
  map_dfc(1:4, ~tibble(x = as.integer(tsx[[.x]]), y = c(24, 19, 17.5, 14, 12, rep(0, 205)), z = c(3, 4, 7, 4.5, 5, rep(0, 205))) %>%
      pmap(., apply_rnorm) %>%
      unlist() %>%
      c(rep(NA, 10000 - length(.)), .)) %>%
  mutate(pick_1 = rnorm(10000, 24, 3)) %>%
  clean_names() %>%
  select(pick_1, pick_2 = x1, pick_3 = x2, pick_4 = x3, pick_5 = x4)

new_pick_values %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.character(parse_number(name))) %>%
  ggplot() +
  geom_density_ridges(aes(x = value, y = name, fill = name), alpha = 0.75, bandwidth = 1) +
  scale_fill_manual(values = rev(multiple_colors(5))) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none') +
  labs(x = '\n7-year predicted WAR', y = 'Pick\n')

# Save the plot as a PNG file
ggsave(
  filename = 'draft-probabilities-4-3.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5.5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320,  # Set the resolution of the plot
  bg = 'white'
)



pick_difference_4_5 <- sample(new_pick_values$pick_4, 10000) - sample(new_pick_values$pick_5, 10000)
mean(pick_difference_4_5, na.rm = TRUE)
mean(pick_difference_4_5 > 0, na.rm = TRUE)
quantile(pick_difference_4_5, na.rm = TRUE)

ggplot() +
  geom_vline(xintercept = mean(pick_difference_4_5, na.rm = TRUE), alpha = 0.5, col = 'white', linetype = 'dashed', lwd = 1) +
  geom_density(aes(pick_difference_4_5), alpha = 0.75, fill = single_color, bandwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 22, y = 0.045, label = "Expected Difference:\n 2.72 WAR", col = 'white', size = 3) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    legend.position = 'none'
  ) +
  labs(x = '\n7-year predicted WAR') +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'draft-probabilities-4-4.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5.5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320,  # Set the resolution of the plot
  bg = 'white'
)

