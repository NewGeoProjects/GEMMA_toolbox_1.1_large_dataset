

## INPUTS ----------------------------------------------------------------------
m_factor <- as.numeric(pcs_m) # Moltiplicator factor for Eigenvectors
clusterized <- ca_flag
env_pca <- df_pca
pcs_to_plot <- retained_selected

# Create colormap
ca_cmap <- c("red", "purple", "orange", "brown", "green", "blue", "pink", 
             "brown", "gray", "lightblue", "darkred", "darkblack", "yellow", 
             "darkgreen", "darkblue", "gold", "black", "beige", 
             "darkgray", "magenta")

# Extract env PCs from PCA
env_pcs <- env_pca$x

## Check cluster ---------------------------------------------------------------
if (clusterized == TRUE){
  # print("CA was performed!")
  # Select the first k colors, where k is the cluster number
  ca_clust_vector <- ca_vector
  ca_colors <- ca_cmap[1:max(ca_vector)]
} else{
  # print("CA was NOT performed!")
  row_names <- rownames(env_pcs)
  ca_clust_vector <- rep(1, length(row_names))
  ca_colors <- ca_cmap[1]
}

## PLOTS =======================================================================
if (length(pcs_to_plot)==2){ # Minimum 2 PCs
  # print("Only 2 PCs selected to biplot")
  # Prepare sample dataset for plots--------------------------------------------
  samples_df <- data.frame(
    x = env_pcs[, pcs_to_plot[1]],
    y = env_pcs[, pcs_to_plot[2]],
    cluster = ca_clust_vector,
    #label = rownames(env_pcs),
    stringsAsFactors = FALSE
  )
  rotation_df <- data.frame(
    x = m_factor*env_pca$rotation[, pcs_to_plot[1]],
    y = m_factor*env_pca$rotation[, pcs_to_plot[2]],
    )# label = rownames(env_pca$rotation))

  # PLOT PC_n°1 vs PC_n°2 ------------------------------------------------------
  pca_biplot_1 <- ggplot() +
    geom_point(data = samples_df, aes(x = x, y = y, 
                                      color = factor(ca_clust_vector), 
                                      fill = factor(ca_clust_vector)),
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, aes(x = x, y = y, label = label),
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(pcs_to_plot[1], " (", round(summary(env_pca)$importance[2, pcs_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(pcs_to_plot[2], " (", round(summary(env_pca)$importance[2, pcs_to_plot[2]] * 100, 2), "%)")) +
    ggtitle(paste0(pcs_to_plot[1], " vs ", pcs_to_plot[2])) +
    # Species arrows
    geom_segment(data = rotation_df, 
                 aes(x = 0, y = 0, xend = x, yend = y), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black", linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, 
              aes(x = x, y = y, label = rownames(rotation_df)), 
              size = 4, hjust = 0, vjust = 0, color = "blue") +
    # Theme
    theme_minimal() +
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)

  # RETURN only PC_n°1 vs PC_n°2 -----------------------------------------------
  outputs <- list(pca_biplot_1)

} else if (length(pcs_to_plot) == 3){# Maximum 3 PCs
  # print("3 PCs selected to biplot")
  # Prepare sample dataset for plots--------------------------------------------
  samples_df <- data.frame(
    x = env_pcs[, pcs_to_plot[1]],
    y = env_pcs[, pcs_to_plot[2]],
    z = env_pcs[, pcs_to_plot[3]],
    cluster = ca_clust_vector,
    #label = rownames(env_pcs),
    stringsAsFactors = FALSE)
  
  rotation_df <- data.frame(
    x = m_factor*env_pca$rotation[, pcs_to_plot[1]],
    y = m_factor*env_pca$rotation[, pcs_to_plot[2]],
    z = m_factor*env_pca$rotation[, pcs_to_plot[3]],
    label = rownames(env_pca$rotation))
  
  # PLOT PC_n°1 vs PC_n°2-------------------------------------------------------
  pca_biplot_1 <- ggplot() +
    # Sample points
    geom_point(data = samples_df, 
               aes(x = x, y = y, color = factor(ca_clust_vector), fill = factor(ca_clust_vector)), 
               alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    # geom_text(data = samples_df, 
    #           aes(x = x, y = y, label = label), 
    #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(pcs_to_plot[1], " (", round(summary(env_pca)$importance[2, pcs_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(pcs_to_plot[2], " (", round(summary(env_pca)$importance[2, pcs_to_plot[2]] * 100, 2), "%)")) +
    ggtitle(paste0(pcs_to_plot[1], " vs ", pcs_to_plot[2])) +
    # Species arrows
    geom_segment(data = rotation_df,aes(x = 0, y = 0, xend = x, yend = y),arrow = arrow(length = unit(0.2, "cm")), color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, aes(x = x, y = y, label = rownames(rotation_df)), size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)
  
  
  # PLOT PC_n°1 vs PC_n°3-------------------------------------------------------
  pca_biplot_2 <- ggplot() +
    geom_point(data = samples_df, aes(x = x, y = z, color = factor(ca_clust_vector), fill = factor(ca_clust_vector)), alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
   # geom_text(data = samples_df, aes(x = x, y = z, label = label), size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(pcs_to_plot[1], " (", round(summary(env_pca)$importance[2, pcs_to_plot[1]] * 100, 2), "%)")) +
    ylab(paste0(pcs_to_plot[3], " (", round(summary(env_pca)$importance[2, pcs_to_plot[3]] * 100, 2), "%)")) +
    ggtitle(paste0(pcs_to_plot[1], " vs ", pcs_to_plot[3])) +
    # Species arrows
    geom_segment(data = rotation_df,aes(x = 0, y = 0, xend = x, yend = z),arrow = arrow(length = unit(0.2, "cm")), color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, aes(x = x, y = z, label = rownames(rotation_df)), size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)
  
  # PLOT PC_n°2 vs PC_n°3 --------------------------------------------------------
  pca_biplot_3 <- ggplot() +
    geom_point(data = samples_df, aes(x = y, y = z, color = factor(ca_clust_vector), fill = factor(ca_clust_vector)), alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
    #geom_text(data = samples_df, aes(x = y, y = z, label = label), size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
    xlab(paste0(pcs_to_plot[2], " (", round(summary(env_pca)$importance[2, pcs_to_plot[2]] * 100, 2), "%)")) +
    ylab(paste0(pcs_to_plot[3], " (", round(summary(env_pca)$importance[2, pcs_to_plot[3]] * 100, 2), "%)")) +
    ggtitle(paste0(pcs_to_plot[2], " vs ", pcs_to_plot[3])) +
    # Species arrows
    geom_segment(data = rotation_df,aes(x = 0, y = 0, xend = y, yend = z),arrow = arrow(length = unit(0.2, "cm")), color = "black",linewidth = 0.5) +
    # Species labels
    geom_text(data = rotation_df, aes(x = y, y = z, label = rownames(rotation_df)), size = 4, hjust = 0, vjust = 0, color = "blue")+
    # Theme
    theme_minimal()+
    # Color mapping
    scale_color_manual(name = "Clusters", values = ca_colors) +
    scale_fill_manual(name = "Clusters", values = ca_colors)
  
  # PLOT 3D---------------------------------------------------------------------
  # Create a PCA scatter plot
  pca_plot_3d <- plot_ly()
  # Add scatterplot for sample data
  pca_plot_3d <- pca_plot_3d %>%
    add_trace(data = samples_df, 
              type = "scatter3d", mode = "markers", 
              x = ~x, y = ~y, z = ~z, 
              color = ~ca_clust_vector, colors = ca_colors, 
              marker = list(size = 8, opacity = 0.8, 
                            line = list(width = 1, color = "DarkSlateGrey")),  
              name = "Sample data", showlegend = TRUE)
  # Add labels for sample data
  # pca_plot_3d <- pca_plot_3d %>%
  #   add_trace(data = samples_df, type = "scatter3d", mode = "text", 
  #             x = ~x, y = ~y, z = ~z, 
  #             text = ~label, textfont = list(size = 12, color = "black"),  
  #             name = "Id labels", showlegend = TRUE)
  # Add arrows
  for (i in seq_len(nrow(rotation_df))) {
    pca_plot_3d <- pca_plot_3d %>%
      add_trace(type = "scatter3d", mode = "lines",
                x = c(0, rotation_df$x[i]),
                y = c(0, rotation_df$y[i]),
                z = c(0, rotation_df$z[i]),
                line = list(color = "black", width = 2),
                name = paste("Arrow", i),
                showlegend = FALSE)}
  #Add labels for sample data
  pca_plot_3d <- pca_plot_3d %>%
    add_trace(data = rotation_df, type = "scatter3d", mode = "text",
              x = ~x, y = ~y, z = ~z,
              text = ~label,
              textfont = list(size = 12, color = "blue"),
              name = "Eigen vector names",
              showlegend = FALSE)
  Layout
  pca_plot_3d <- pca_plot_3d %>%
    layout(
      scene = list(
        xaxis = list(title = paste0(pcs_to_plot[1], " (", round(summary(env_pca)$importance[2, pcs_to_plot[1]] * 100, 2), "%)")),
        yaxis = list(title = paste0(pcs_to_plot[2], " (", round(summary(env_pca)$importance[2, pcs_to_plot[2]] * 100, 2), "%)")),
        zaxis = list(title = paste0(pcs_to_plot[3], " (", round(summary(env_pca)$importance[2, pcs_to_plot[3]] * 100, 2), "%)"))
      ),
      margin = list(l = 0, r = 0, b = 0, t = 0), 
      showlegend = TRUE, legend = list(x = 0, y = 1)
    )
  # Save 1vs2 + 1vs3 + 2vs3 + 3D in list----------------------------------------
  outputs <- list(pca_biplot_1, pca_biplot_2, pca_biplot_3, pca_plot_3d)
}

# RETURN========================================================================
outputs



