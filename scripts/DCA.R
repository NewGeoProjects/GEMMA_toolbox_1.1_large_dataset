

## INPUT =======================================================================
env_raw <- DCA_env
env_raw <- decostand(sqrt(env_raw), "hellinger")
env_molecules <- env_raw

## PERFORM DCA =================================================================
dca_result <- decorana(env_molecules)
env_comps <- dca_result$rproj

## DETERMINATION OF NUM SIGNIFICATIVE DCAs =====================================
# Proportion of variance expalined
prop_var <- dca_result$evals.decorana^2 / sum(dca_result$evals.decorana^2)
# Create a data frame for ggplot
df <- data.frame(DCA = 1:length(prop_var), Prop_Var = prop_var)

# Create the ggplot
pve_plot <- ggplot(df, aes(x = reorder(colnames(env_comps), -prop_var), 
                           y = Prop_Var)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(xintercept = df[which(diff(diff(df$Prop_Var)) < 0)[1], 1], 
             color = "red", linetype = "dashed") +
  xlab("Detrended Component") +
  ylab("Proportion of Variance Explained") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# RETURN =======================================================================
list(pve_plot, env_comps)
