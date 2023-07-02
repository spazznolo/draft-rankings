
max(full_ranking_matrix - 1)
# Fit the Plackett-Luce model
pl_model <- PlackettLuce(unname(full_ranking_matrix[,2:210] - 1), weights = final_weights, npseudo = 0, maxit = c(5000, 100))
summary(pl_model)

unname(full_ranking_matrix)
# Obtain maximum likelihood estimates from the Plackett-Luce model
mle_estimates <- coef(pl_model, log = FALSE)
mle_estimates

hist(exp(rnorm(1000, -2.2, 0.5137)))
