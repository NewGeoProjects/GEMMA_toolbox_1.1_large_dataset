
## INPUTS ======================================================================
m_factor <- as.numeric(rds_m)
predictors <- rda_predictor_env
community <- rda_community_env
clusterized <- ca_flag
rds_to_plot <- retained_selected 
# Create colormap
ca_cmap <- c("red", "purple", "orange", "brown", "green", "blue", "pink", 
             "brown", "gray", "lightblue", "darkred", "darkblack", "yellow", 
             "darkgreen", "darkblue", "gold", "black", "beige", 
             "darkgray", "magenta")

# RUN RDA ======================================================================
myrda <- rda(community ~ ., data = predictors)

## SCREEPLOT====================================================================
# RDAs Proportion of variance explained (PVE)___________________________________
percent_explained <- as.matrix(
  round((summary(myrda)$concont$importance[2,]*100), 2))
percent_explained <- t(percent_explained)

# Create df for ggplot__________________________________________________________
plot_data <- data.frame(
  PrincipalComponent = 1:length(percent_explained),
  ProportionExplained = percent_explained[1,]
)
# Create plot___________________________________________________________________
screeplot <- ggplot(plot_data, 
                    aes(x = reorder(rownames(plot_data),-ProportionExplained), 
                        y = ProportionExplained)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "RDA SCREEPLOT",
       x = "RDA compontents",
       y = "Proportion of Variance Explained")
  theme_minimal()

## HEATMAP =====================================================================
# corr matrix
biplot_matr <- as.matrix(myrda$CCA$biplot)
# Convert the matrix to a data frame using melt()
biplot_df <- melt(biplot_matr)
# Plot the heatmap with cell values as labels using ggplot2
heatmap <- ggplot(biplot_df,
       aes(x = Var2, y = Var1, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, 
                       name = "Scores") +
  labs(title = "Heatmap RDA",
       x = "RDAs",
       y = "PREDICTORS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Update outputlist
outputs_1 <- list(screeplot, heatmap)

## BIPLOTS =====================================================================
## RDA SUMMARY
summary <- summary(myrda)

## EXTRACT BIPLOTS DATA FROM RDA RESULT
env_rds <- myrda$CCA$wa

## Check cluster ===============================================================
if (clusterized == TRUE){
  # print("CA was performed!")
  # Select the first k colors, where k is the cluster number
  ca_clust_vector <- ca_vector
  ca_colors <- ca_cmap[1:max(ca_vector)]
} else{
  # print("CA was NOT performed!")
  row_names <- rownames(env_rds)
  ca_clust_vector <- rep(1, length(row_names))
  ca_colors <- ca_cmap[1]
}


## PLOTS========================================================================
if (length(rds_to_plot)==2){ # Minimum 2 PCs
  # print("Only 2 RDs selected to biplot")
  # Prepare sample dataset for plots--------------------------------------------
  samples_df <- data.frame(
    x = env_rds[, rds_to_plot[1]],
    y = env_rds[, rds_to_plot[2]],
    cluster = ca_clust_vector,
    label = rownames(env_rds),
    stringsAsFactors = FALSE
  )
  rotation_df <- data.frame(
    x = m_factor*myrda$CCA$biplot[, rds_to_plot[1]],
    y = m_factor*myrda$CCA$biplot[, rds_to_plot[2]],
    label = rownames(myrda$CCA$biplot)
  )
  # PLOT RDA_n°1 vs RDA_n°2-----------------------------------------------------
  rda_biplot_1 <- ggplot() +
    geom_point(data = samples_df, aes(x = x, y = y, 
                                      color = factor(ca_clust_vector), 
                                      fill = factor(ca_clust_vector)),
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, aes(x = x, y = y, label = label),
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(rds_to_plot[1], " (", round(summary$concont$importance[2, rds_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(rds_to_plot[2], " (", round(summary$concont$importance[2, rds_to_plot[2]] * 100, 2), "%)")) +
    ggtitle("RDA plot") +
    # Species arrows
    geom_segment(data = rotation_df,
                 aes(x = 0, y = 0, xend = x, yend = y),
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, 
              aes(x = x, y = y, label = rownames(rotation_df)), 
              size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)
  
  # RETURN only RDA_n°1 vs RDA_n°2 -----------------------------------------------
  outputs_2 <- list(rda_biplot_1)

} else if (length(rds_to_plot)==3){# Maximum 3 PCs
  # print("3 RDAs selected to biplot")
  # Prepare sample dataset for plots--------------------------------------------
  samples_df <- data.frame(
    x = env_rds[, rds_to_plot[1]],
    y = env_rds[, rds_to_plot[2]],
    z = env_rds[, rds_to_plot[3]],
    cluster = ca_clust_vector,
    label = rownames(env_rds),
    stringsAsFactors = FALSE
  )
  rotation_df <- data.frame(
    x = m_factor*myrda$CCA$biplot[, rds_to_plot[1]],
    y = m_factor*myrda$CCA$biplot[, rds_to_plot[2]],
    z = m_factor*myrda$CCA$biplot[, rds_to_plot[3]],
    label = rownames(myrda$CCA$biplot)
  )

  # PLOT RDA_n°1 vs RDA_n°2 ----------------------------------------------------
  rda_biplot_1 <- ggplot() +
    geom_point(data = samples_df, aes(x = x, y = y, 
                                      color = factor(ca_clust_vector), 
                                      fill = factor(ca_clust_vector)),
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, aes(x = x, y = y, label = label),
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(rds_to_plot[1], " (", round(summary$concont$importance[2, rds_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(rds_to_plot[2], " (", round(summary$concont$importance[2, rds_to_plot[2]] * 100, 2), "%)")) +
    ggtitle("RDA plot") +
    # Species arrows
    geom_segment(data = rotation_df,
                 aes(x = 0, y = 0, xend = x, yend = y),
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df,
              aes(x = x, y = y, label = rownames(rotation_df)), 
              size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)
  

  # PLOT RDA_n°1 vs RDA_n°3 ----------------------------------------------------
  rda_biplot_2 <- ggplot() +
    geom_point(data = samples_df, aes(x = x, y = z, color = factor(ca_clust_vector), fill = factor(ca_clust_vector)),
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, aes(x = x, y = z, label = label),
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(rds_to_plot[1], " (", round(summary$concont$importance[2, rds_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(rds_to_plot[3], " (", round(summary$concont$importance[2, rds_to_plot[3]] * 100, 2), "%)")) +
    ggtitle("RDA plot") +
    # Species arrows
    geom_segment(data = rotation_df,
                 aes(x = 0, y = 0, xend = x, yend = z),
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, 
              aes(x = x, y = z, label = rownames(rotation_df)), 
              size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)

  # PLOT RDA_n°2 vs RDA_n°3 ----------------------------------------------------
  rda_biplot_3 <- ggplot() +
    geom_point(data = samples_df, aes(x = y, y = z, 
                                      color = factor(ca_clust_vector), 
                                      fill = factor(ca_clust_vector)),
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, aes(x = y, y = z, label = label),
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(rds_to_plot[2], " (", round(summary$concont$importance[2, rds_to_plot[2]] * 100, 2), "%)")) +
    ylab(paste0(rds_to_plot[3], " (", round(summary$concont$importance[2, rds_to_plot[3]] * 100, 2), "%)")) +
    ggtitle("RDA plot") +
    # Species arrows
    geom_segment(data = rotation_df,
                 aes(x = 0, y = 0, xend = y, yend = z),
                 arrow = arrow(length = unit(0.2, "cm")), color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, 
              aes(x = y, y = z, label = rownames(rotation_df)), 
              size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)

  # PLOT 3D---------------------------------------------------------------------
  # Create a rda scatter plot
  rda_plot_3d <- plot_ly()
  # Add scatterplot for sample data
  rda_plot_3d <- rda_plot_3d %>%
    add_trace(data = samples_df, type = "scatter3d", mode = "markers", 
              x = ~x, y = ~y, z = ~z, 
              #text = ~label, 
              color = ~ca_clust_vector, colors = ca_colors,
              marker = list(size = 8, opacity = 0.8, 
                            line = list(width = 1, color = "DarkSlateGrey")),  
              name = "Sample data", showlegend = TRUE)
  # Add labels for sample data
  rda_plot_3d <- rda_plot_3d %>%
    add_trace(data = samples_df, type = "scatter3d", mode = "text", 
              x = ~x, y = ~y, z = ~z, 
              #text = ~label, textfont = list(size = 12, color = "black"),  
              name = "Id labels", showlegend = TRUE)
  # Add arrows
  for (i in seq_len(nrow(rotation_df))) {
    rda_plot_3d <- rda_plot_3d %>%
      add_trace(type = "scatter3d", mode = "lines",
                x = c(0, rotation_df$x[i]),
                y = c(0, rotation_df$y[i]),
                z = c(0, rotation_df$z[i]),
                line = list(color = "black", width = 2),
                name = paste("Arrow", i),
                showlegend = FALSE)
  }
  # Add labels for sample data
  rda_plot_3d <- rda_plot_3d %>%
    add_trace(data = rotation_df, type = "scatter3d", mode = "text", 
              x = ~x, y = ~y, z = ~z,
              text = ~label,
              textfont = list(size = 12, color = "blue"), 
              name = "Eigen vector names",
              showlegend = TRUE)
  # Layout
  rda_plot_3d <- rda_plot_3d %>%
    layout(
      scene = list(
        xaxis = list(title = paste0(rds_to_plot[1], " (", round(summary$concont$importance[2, rds_to_plot[1]] * 100, 2), "%)")),
        yaxis = list(title = paste0(rds_to_plot[2], " (", round(summary$concont$importance[2, rds_to_plot[2]] * 100, 2), "%)")),
        zaxis = list(title = paste0(rds_to_plot[3], " (", round(summary$concont$importance[2, rds_to_plot[3]] * 100, 2), "%)"))
      ),
      margin = list(l = 0, r = 0, b = 0, t = 0), showlegend = TRUE, legend = list(x = 0, y = 1)
    )
  # RETURN only PC_n°1 vs PC_n°2 -----------------------------------------------
  outputs_2 <- list(rda_biplot_1,
                  rda_biplot_2,
                  rda_biplot_3,
                  rda_plot_3d)

}


# RETURN========================================================================
outputs <- append(outputs_1, outputs_2)
outputs

