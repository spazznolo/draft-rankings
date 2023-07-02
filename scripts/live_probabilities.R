
source('~/Documents/projects/twitter_authentication.R')

live_picks <- c('BEDARD, CONNOR', 'CARLSSON, LEO', 'FANTILLI, ADAM', 'SMITH, WILLIAM', 'REINBACHER, DAVID',
                'SIMASHEV, DMITRIY', 'MICHKOV, MATVEI', 'LEONARD, RYAN', 'DANIELSON, NATE', 'DVORSKY, DALIBOR',
                'WILLANDER, TOM', 'BUT, DANIL', 'BENSON, ZACH', 'YAGER, BRAYDEN', 'WOOD, MATTHEW',
                'HONZEK, SAMUEL', 'SANDIN PELLIKKA, AXEL', 'BARLOW, COLBY', 'MOORE, OLIVER', 'SALE, EDUARD',
                'STRAMEL, CHARLIE', 'BONK, OLIVER', 'PERREAULT, GABRIEL', 'MOLENDYK, TANNER',
                'STENBERG, OTTO', 'MUSTY, QUENTIN', 'RITCHIE, CALUM', 'COWAN, EASTON', 'LINDSTEIN, THEO',
                'NADEAU, BRADLY', 'GULYAYEV, MIKHAIL', 'EDSTROM, DAVID', 'MYATOVIC, NICO', 'BRINDLEY, GAVIN',
                'GAJAN, ADAM', 'HALTTUNEN, KASPER', 'GAUTHIER, ETHAN', 'HRABAL, MICHAEL', 'WAHLBERG, ANTON',
                'CRISTALL, ANDREW', 'AUGUSTINE, TREY', 'GIBSON, ANDREW', 'NILSSON, FELIX', 'KANTSEROV, ROMAN',
                'STRBAK, MAXIM', 'LIND, KALAN', 'CLEVELAND, BRADY', 'MORIN, ETIENNE', 'NELSON, DANNY',
                'REHKOPF, CARSON', 'BJARNASON, CARSON', 'MOLGAARD, OSCAR FISKER', 'KUMPULAINEN, RASMUS',
                'DVORAK, JAKUB', 'MISIAK, MARTIN', 'AKEY, BEAU', 'DRAGICEVIC, LUKAS', 'HAMEENAHO, LENNI',
                'TERRANCE, CAREY', 'CLARA, DAMIAN', 'BERTUCCI, TRISTAN', 'UNGER SORUM, FELIX',
                'SAWCHYN, GRACYN', 'HEIDT, RILEY')

prospects_taken <- which(skater_dictionary$Skater %in% live_picks)

probability_of_pick <-
  weighted_frequentist_probabilities %>%
  group_by(Skater) %>%
  mutate(prediction = cumsum(probability)) %>%
  ungroup() %>%
  filter(rank == length(live_picks), Skater == live_picks[length(live_picks)]) %>%
  pull(prediction)

live_pre_draft_rank <- skater_dictionary %>% filter(Skater == live_picks[length(live_picks)]) %>% pull(pre_draft_rank)
rank_among_remaining <- skater_dictionary %>% filter(!(Skater %in% live_picks) & pre_draft_rank < live_pre_draft_rank) %>% nrow()
rank_among_remaining <- rank_among_remaining + 1

skater_list <- 1:skaters
live_skaters <- skater_list[-prospects_taken]
skaters_left <- length(live_skaters)
live_mle_estimates <- mle_estimates[-prospects_taken]

# Simulate draft rankings
live_draft_simulations <- 
  replicate(50000, sample(live_skaters, skaters_left, replace = FALSE, prob = live_mle_estimates)) %>%  # Simulate 100,000 drafts based on the estimated probabilities
  matrix(., nrow = skaters_left, ncol = 50000)  # Convert the simulated drafts to a matrix format

# Calculate probabilities for each rank for each skater
list_of_probabilities <- map(live_skaters, ~rowSums(live_draft_simulations == .) / 50000)

new_skater_dictionary <-
  skater_dictionary %>%
  filter(!(skater_id %in% prospects_taken))

# Create a data frame with draft probabilities
remaining_frequentist_probabilities <-
  do.call(rbind, list_of_probabilities) %>%  # Combine the probability lists into a single matrix
  as_tibble() %>%  # Convert the matrix to a tibble
  bind_cols(new_skater_dictionary, .) %>%
  arrange(desc(V1)) %>%
  select(Skater, V1)

tweet_text <-
  paste0(
    "PICK ", length(live_picks), ": ", live_picks[length(live_picks)], "\n", "\n",
    "Pre-draft rank: ", live_pre_draft_rank, "\n",
    "Rank among remaining prospects: ", rank_among_remaining, "\n",
    "Probability of being drafted this early: ", round(100*probability_of_pick, 2), "%", "\n", "\n",
    "Next:")

remaining_frequentist_probabilities %>%
  slice(1:10) %>%
  mutate(skater = sub("(\\w+),\\s(\\w+)","\\2 \\1", Skater)) %>%
  ggplot() +
  geom_bar(aes(reorder(Skater, V1), V1), stat = 'identity', fill = 'orchid4', col = 'black', alpha = 0.75) +
  coord_flip() +
  theme_minimal() +
  ggpubr::rremove('ylab') +
  ggpubr::rremove('xlab') +
  labs(
    title = paste0('\nTop selection probabilities for pick ', length(live_picks) + 1),
    subtitle = bquote("Data from the NHL Draft Tool 2023. Created by"~bold("@iyer_prashanth")~and~bold("@spazznolo."))) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    axis.text.x = element_text(face = "bold", color = '#202020'),
    axis.text.y = element_text(face = "bold", color = '#202020'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white'),
    plot.subtitle = element_text(size = 9, hjust = 1.50)
  )

# Save the plot as a PNG file
ggsave(
  filename = 'pick-probability-test.png',  # Specify the file name
  path = '/Users/ada/Downloads',  # Specify the file path
  width = 5.5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320,  # Set the resolution of the plot
  bg = 'white'
)

tw <- updateStatus(tweet_text, mediaPath = '/Users/ada/Downloads/pick-probability-test.png')


#deleteStatus(tw)

#tw <- updateStatus('Top pick probabilities going into Round 2:', mediaPath = '/Users/ada/Downloads/pick-probability-test.png')

