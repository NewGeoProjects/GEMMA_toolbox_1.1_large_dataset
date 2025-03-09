

## INPUTS ======================================================================
env_molecules <- pca_env
num_permutation <- perm_num
retained_pcs_number <- pcs_retain

# PERFORM PCA  =================================================================
env_pca <- prcomp(env_molecules, center = TRUE, scale. = TRUE)

# Extract the principal components
env_pcs <- env_pca$x
# Retain PCs
retained_pcs <-  env_pcs[, 1:retained_pcs_number]

## MONTECARLO TEST =============================================================
## OBSERVED DF__________________________________________________________________
# Extract std
std_dev_obs <- env_pca$sdev[1:retained_pcs_number] 
# Variance explained of each component
var_obs_explained <- round(std_dev_obs^2/sum(env_pca$sdev^2)*100,2)
# Total variance explained
total_variance_obs <- sum(var_obs_explained)

## RANDOM LOOP TEST_____________________________________________________________
## Define the maximum and minimum values (for random dataframe)
min_value <- min(env_molecules)
max_value <- max(env_molecules)
# Init vector to store perm test statistics
simulated_statistics <- numeric(num_permutation)
# Create an empty matrix with 3 columns and num_permutations rows
var_sim_explained <- matrix(NA, nrow = num_permutation, ncol = retained_pcs_number)
# Perform permutations
your_data <- env_molecules
set.seed(42)
for (i in 1: num_permutation){
  # Create a data random dataframe
  permuted_pcs <- as.data.frame(
    matrix(runif(
      n = ncol(your_data) * nrow(your_data),
      min = min_value,
      max = max_value),
      nrow = nrow(your_data)))
  # Perform PCA on random df
  pca_result_sim <- prcomp(permuted_pcs, center = TRUE, scale. = TRUE)
  # variance explained of each component
  var_sim_explained[i,] <- round(
    pca_result_sim$sdev[1:retained_pcs_number]^2/sum(pca_result_sim$sdev^2)*100,2)
  # Total variance explained
  simulated_statistics [i] <- sum(pca_result_sim$sdev[1:retained_pcs_number]^2)
}

## PLOT ========================================================================
## Convert the matrix to data frames
df_obs <- as.data.frame(var_obs_explained)
df_sim <- as.data.frame(var_sim_explained)
## Observed df
colnam <- colnames(env_pca$x)
summary_data_obs <- data.frame(
  variable = colnam[1:retained_pcs_number],
  x = rep(1:retained_pcs_number),
  mean = df_obs[,],
  type = "observed"
)
## Simulated df
summary_data_sim <- data.frame(
  variable = colnam[1:retained_pcs_number],
  x = rep(1:retained_pcs_number),
  mean = colMeans(df_sim),
  type = "simulated"
)

# Combine the two data frames
combined_summary_data <- rbind(summary_data_obs, summary_data_sim)

# Plotting
pvals_plot <- ggplot(combined_summary_data, aes(x=x, y=mean, color = type)) +
  geom_point() +
  geom_line(aes(x = x, y = mean, color = type)) +
  labs(title = "PCA MONTECARLO",
       x = "Principal componets",
       y = "Variance explained [%]") +
  theme_minimal()

# RETURN------------------------------------------------------------------------
list(pvals_plot, env_pca)
