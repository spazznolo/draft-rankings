
# step 1: simulate up to wings first pick (assume pick 9)
# step 2: remove 8 chosen prospects from analysis
# step 3: sample pick from remaining prospects ranked from 9:20
# step 4: save pick probabilities for pick 17
# step 5: multiply probabilities in step 4 by sampled prospect value in step 3
# step 6: save values
# step 7: repeat steps 3:6 for each prospect ranked 9:20

mle_estimates












predicted_values <-
  tibble(
    bedard = rnorm(10000, 24, 3),
    fantilli = rnorm(10000, 19, 4),
    michkov = rnorm(10000, 17.5, 7),
    carlsson = rnorm(10000, 15, 4.5),
    smith = rnorm(10000, 13, 5))

predicted_picks <-
  tibble(
    michkov = sample(3:5, 100, replace = TRUE, prob = c(0.25, 0.30, 0.45)),
    carlsson = sample(3:5, 100, replace = TRUE, prob = c(0.65, 0.20, 0.15)),
    smith = sample(3:5, 100, replace = TRUE, prob = c(0.10, 0.47, 0.40))
    )

# draft michkov
value_3 <- predicted_values$michkov

remaining_5 <- sample(c('smith', 'carlsson'), 1000, replace = TRUE, prob = c(0.85, 0.15))
part_1 <- sum(remaining_5 == 'smith')
part_2 <- sum(remaining_5 == 'carlsson')
value_5 <- c(sample(predicted_values$smith, part_1), sample(predicted_values$carlsson, part_2))

mean(value_3 + value_5)


# draft carlsson
value_3 <- predicted_values$carlsson

remaining_5 <- sample(c('smith', 'michkov'), 1000, replace = TRUE, prob = c(0.40, 0.60))
part_1 <- sum(remaining_5 == 'smith')
part_2 <- sum(remaining_5 == 'michkov')
value_5 <- c(sample(predicted_values$smith, part_1), sample(predicted_values$michkov, part_2))

mean(value_3 + value_5)

# draft smith
value_3 <- predicted_values$smith

remaining_5 <- sample(c('carlsson', 'michkov'), 1000, replace = TRUE, prob = c(0.20, 0.80))
part_1 <- sum(remaining_5 == 'carlsson')
part_2 <- sum(remaining_5 == 'michkov')
value_5 <- c(sample(predicted_values$carlsson, part_1), sample(predicted_values$michkov, part_2))

mean(value_3 + value_5)





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




