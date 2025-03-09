
# ## INPUT________________________________________________________________________
env_raw <- DCA_env
retained_dcs_number <- DCAs_retain
num_permutation <- dca_per_num

## STANDADIZE environmental variables___________________________________________
env <- decostand(sqrt(env_raw), "hellinger")

## PERFORM DCA _________________________________________________________________
dca_result <- decorana(env)

# Retain DCs
dca_scores <- dca_result$rproj
retained_dcs <-  dca_scores[, 1:retained_dcs_number]

## MONTECARLO TEST =============================================================
## OBSERVED DF__________________________________________________________________
# Extract variance explained of each component
dca_eigvals_percent <- round(
  (dca_result$evals.decorana / sum(scores(dca_result$evals.decorana)))*100, 2)
var_obs_explained <- dca_eigvals_percent[1:retained_dcs_number]

# Total variance explained
total_variance_obs <- sum(var_obs_explained)

## LOOP ------------------------------------------------------------------------
# Define the maximum and minimum values (for random dataframe)
min_value <- min(env_raw)
max_value <- max(env_raw)
# Init vector to store perm test statistics
simulated_statistics <- numeric(num_permutation)
# Create an empty matrix with 3 columns and num_permutations rows
var_sim_explained <- matrix(NA, nrow = num_permutation, ncol = retained_dcs_number)
# Perform permutations
your_data <- env_raw
set.seed(42)
for (i in 1: num_permutation){
  ## Create a data random dataframe (same dimensions of retained_pcs df)
  permuted_pcs <- as.data.frame(
    matrix(runif(
      n = ncol(your_data) * nrow(your_data),
      min = min_value,
      max = max_value),
      nrow = nrow(your_data)))
  ## Perform PCA on random df
  pca_result_sim <- prcomp(permuted_pcs, center = TRUE, scale. = TRUE)
  ## Calc variance explained of each component
  var_sim_explained[i,] <- pca_result_sim$sdev[1:retained_dcs_number]^2
  ## Calculate the total variance explained
  simulated_statistics [i] <- sum(pca_result_sim$sdev[1:retained_dcs_number]^2)
}

# # calculate p value_____________________________________________________________
# p_value <- mean(simulated_statistics >= total_variance_obs)
# if (p_value > 0.05){
#   cat("Fail, p:", p_value)
# }else{
#   print("Test passed!")
# }


## PLOT ========================================================================
## Convert the matrix to data frames
df_obs <- as.data.frame(var_obs_explained[])
df_sim <- as.data.frame(var_sim_explained)
## Calculate means
means_sim <- colMeans(df_sim)
## Combine means and standard deviations into data frames
summary_data_obs <- data.frame(
  variable = rownames(df_obs),
  x = rep(1:retained_dcs_number),
  mean = df_obs[,1],
  type = "observed"
)
summary_data_sim <- data.frame(
  variable = colnames(df_sim),
  x = rep(1:retained_dcs_number),
  mean = means_sim,
  type = "simulated"
)

# Combine the two data frames
combined_summary_data <- rbind(summary_data_obs, summary_data_sim)
# Plotting
dca_mc_pvals <- ggplot(combined_summary_data, aes(x = x, y = mean, color = type)) +
  geom_point() +
  geom_line(aes(x = x, y = mean, color = type)) +
  labs(title = "DCA MONTECARLO",
       x = "Principal componets",
       y = "Variance explained [%]") +
  theme_minimal()

## RETURN_______________________________________________________________________
list(dca_mc_pvals, dca_scores, dca_result)











####################################################################################
# # Extract DCA scores
# dca_scores <- dca_result$rproj
# print(head(dca_scores))
# 
# # Retain DCs
# print(dca_retain)
# dca_retained_scors <- dca_scores[, 1:dca_retain]
# print(dca_retained_scors)
# 
# ## MONTECARLO___________________________________________________________________
# # Function to calculate the observed test statistic for DCA
# calculate_test_statistic_dca <- function(data) {
#   sum_squares <- apply(data, 2, function(x) sum(x^2))
#   return(sum(sum_squares))
# }
# # Calculate the observed test statistic for DCA
# observed_test_statistic_dca <- calculate_test_statistic_dca(dca_retained_scors)
# # Set the number of Monte Carlo permutations
# p_val_list_dca <- numeric((dca_per_max - dca_per_min) / dca_per_step + 1)
# # Perform Monte Carlo permutations
# for (i in seq(dca_per_min, dca_per_max, by = dca_per_step)) {
#   # print(i)
#   # Permute the eigenvalues
#   permuted_scores <- dca_retained_scors
#   for (j in 1:dca_retain) {
#     permuted_scores[, j] <- sample(permuted_scores[, j])
#   }
#   # Calculate the permuted test statistic
#   permuted_test_statistic_dca <- calculate_test_statistic_dca(permuted_scores)
#   # Calculate p-values
#   p_value <- sum(permuted_test_statistic_dca >= observed_test_statistic_dca)/i
#   p_val_list_dca[(i - dca_per_min) / dca_per_step + 1] <- p_value
# }
# 
# ## PDF FITTING # 1:"normal", 2: "gamma", 3:"log-normal", 4:"exponential"________
# if (dca_pdf_type == 1) { # normal
#   fit <- fitdistr(p_val_list_dca, "normal")
#   mean <- fit$estimate[["mean"]]
#   sd <- fit$estimate[["sd"]]
#   percentile_95 <- qnorm(0.95, mean = mean, sd = sd)
#   # Display
#   cat("Normal distribution p-value\n",
#       "Mean:", mean, "\n",
#       "std:", sd, "\n",
#       "Median:", median(p_val_list_dca), "\n",
#       "95th percentile:", percentile_95)
# } else if (dca_pdf_type == 2) { # gamma
#   fit <- fitdistr(p_val_list_dca, "gamma")
#   shape <- fit$estimate[["shape"]]
#   rate <- fit$estimate[["rate"]]
#   percentile_95 <- qgamma(0.95, shape = shape, rate = rate)
#   # Display
#   cat("Gamma distribution p-value\n",
#       "Shape:", shape, "\n",
#       "Rate:", rate, "\n",
#       "95th percentile:", percentile_95)
# } else if (dca_pdf_type == 3) { # log-normal
#   fit <- fitdistr(p_val_list_dca, "log-normal")
#   meanlog <- fit$estimate[["meanlog"]]
#   sdlog <- fit$estimate[["sdlog"]]
#   percentile_95 <- qlnorm(0.95, meanlog = meanlog, sdlog = sdlog)
#   # Display
#   cat("Log-normal distribution p-value\n",
#       "Meanlog:", meanlog, "\n",
#       "stdlog:", sdlog, "\n",
#       "95th percentile:", percentile_95)
# } else if (dca_pdf_type == 4) { # exponential
#   fit <- fitdistr(p_val_list_dca, "exponential")
#   lambda <- fit$estimate[["rate"]]
#   percentile_95 <- qexp(0.95, rate = lambda)
#   # Display
#   cat("Exponential distribution p-value\n",
#       "Lambda:", lambda, "\n",
#       "95th percentile:", percentile_95)
# }
# 
# 
# ## HISTOGRAM + PDF______________________________________________________________
# # Create a data frame for histogram
# df_hist <- data.frame(p_val = p_val_list_dca)
# 
# ## CREATE PLOT__________________________________________________________________
# # Calculate the dynamic binwidth with 20 breaks
# n_braks <- 20
# x_range <- range(df_hist$p_val)
# binwidth <- diff(x_range) / n_braks
# # ggplot
# dca_mc_pvals <- ggplot(df_hist, aes(x = p_val)) +
#   geom_histogram(binwidth = binwidth, fill = "blue") +
#   geom_vline(xintercept = percentile_95, color = "red", linetype = "dashed") +
#   labs(title = "MONTECARLO test (DCA)",
#        x = "p-values",
#        y = "Density") +
#   theme_minimal()
# # #debug
# # pvals_plot_dca
# 
# ## Evaluate the significance of DCA_____________________________________________
# significance_level_dca <- 0.05
# if (percentile_95 < significance_level_dca) {
#   cat("\nThe DCA scores are statistically significant.\n")
#   cat("Reject the null hypothesis.\n")
# } else {
#   cat("\nThe DCA scores are not statistically significant.\n")
#   cat("Fail to reject the null hypothesis. We do not have enough evidence 
#       to conclude that the observed results are statistically significant.\n")
# }
# 
# ## RETURN_______________________________________________________________________
# list(dca_mc_pvals, dca_scores)



