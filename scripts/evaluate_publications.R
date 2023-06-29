
error_weights <- c(17.1, 12.3, 10.2, 9, 8.2, 7.6, 7.1, 6.7, 6.3, 6.0,
                   5.7, 5.5, 5.3, 5.1, 4.9, 4.7, 4.5, 4.3, 4.2, 4.0,
                   3.9, 3.7, 3.6, 3.5, 3.4, 3.2, 3.1, 3, 2.9, 2.8, 
                   2.7, 2.6, 2.5, 2.4, 2.4, 2.3, 2.2, 2.2, 2.1, 2.0, 
                   2.0, 1.9, 1.9, 1.8, 1.7, 1.7, 1.7, 1.6, 1.6, 1.5, 
                   1.5, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, 1.2, 
                   1.1, 1.1, 1.1, 1.1)

error_weights <- 64*error_weights/sum(error_weights)
relevant_rankings <- which(month(ranking_dictionary$ranking_date) == 6)

ranking_names <-
  tibble(
    publication = tolower(gsub(' ', '', rankings_names))
  ) %>%
  mutate(
    publication = ifelse(publication == 'mckeen', 'mckeens', publication)
  ) %>%
  filter(nchar(publication) > 1) 

error_ranking_dictionary <- ranking_dictionary %>% slice(relevant_rankings)
error_ranking_names <- ranking_names %>% slice(relevant_rankings)

draft_order <- map_vec(live_picks, ~which(skater_dictionary$Skater == .))

preds_skater <-
  skater_dictionary %>%
  arrange(pre_draft_rank) %>%
  pull(skater_id)

pub_and_pred_matrix <-
  rbind(full_ranking_matrix, preds_skater)

mae_pub <-
  map(draft_order, ~apply(pub_and_pred_matrix, 1, function(x) abs(min(74, which(x == .)) - which(draft_order == .)))) %>%
  do.call(cbind, .) %>%
  apply(., 1, function(x) sum(x*error_weights)/length(x))
  
mae_pub <- mae_pub[relevant_rankings]

error_ranking_dictionary %>%
  mutate(pub = error_ranking_names, mae_pub = mae_pub) %>%
  arrange(mae_pub) %>%
  summarize(mean(mae_pub))




