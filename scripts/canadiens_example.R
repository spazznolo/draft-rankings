
weighted_frequentist_probabilities %>%
  filter(Skater %in% c('BEDARD, CONNOR', 'FANTILLI, ADAM', 'MICHKOV, MATVEI', 'CARLSSON, LEO', 'BENSON, ZACH'), 
         rank <= 5) %>%
  group_by(Skater) %>%
  mutate(probability = cumsum(probability)) %>%
  ungroup() %>%
  mutate(probability = 1 - probability) %>%
  mutate(rank = rank + 1) %>%
  pivot_wider(id_cols = rank, names_from = Skater, values_from = probability) %>%
  filter(rank < 6)

(1.0000*24)
(0.002*24) + (0.998*19)
(0.000*24) + (0.215*19) + (0.785*17.5)
(0.000*24) + (0.029*19) + (0.399*17.5) + (0.572*14.0)
(0.000*24) + (0.002*19) + (0.105*17.5) + (0.293*14.0) + (0.600*10.0)