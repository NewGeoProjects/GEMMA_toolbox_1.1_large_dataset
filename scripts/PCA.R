

## INPUT =======================================================================
env_molecules <- pca_env
# Scale and center variables
env_molecules <- decostand(env_molecules, method = "standardize")

## PERFORM PCA =================================================================
env_pca <- prcomp(env_molecules, center = TRUE, scale. = TRUE)
# Extract the principal components
env_pcs <- env_pca$x

## Proportion of variance expalined ============================================
prop_var <- env_pca$sdev^2 / sum(env_pca$sdev^2)
# Create a data frame for ggplot
df <- data.frame(PC = 1:length(prop_var), Prop_Var = prop_var)

# Create the ggplot
pve_plot <- ggplot(df, aes(x = reorder(colnames(env_pca$x), -prop_var), y = Prop_Var)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Principal ComponentS") +
  ylab("Proportion of Variance Explained [%]") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# RETURN
list(pve_plot, env_pcs)
