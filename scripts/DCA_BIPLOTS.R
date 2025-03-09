

## INPUTS ======================================================================
# Create colormap
ca_cmap <- c("red", "purple", "orange", "brown", "green", "blue", "pink",
             "brown", "gray", "lightblue", "darkred", "darkblack", "yellow",
             "darkgreen", "darkblue", "gold", "black", "beige",
             "darkgray", "magenta")
m_factor <- as.numeric(dcs_m)  # Moltiplicator factor for Eigenvectors
clusterized <- ca_flag
env_raw <- DCA_env
dcs_to_plot <- retained_selected

## DCA =========================================================================
## STANDARDIZE ENVIRONMENTAL VARIABLES__________________________________________
env <- decostand(sqrt(env_raw), "hellinger")
## PERFORM DCA__________________________________________________________________
dca_result <- decorana(env)  # Using decorana as DCA function
# EXTRACT SAMPLES POSITIONS VS DCS FROM DCA (POINTS)
dca_scores <- dca_result$rproj
# EXTRACT EIGENVECTORS VS DCS FROM DCA
dca_eigenvectors <- dca_result$cproj
# EXTRACT EIGENVALUES
dca_eigenvalues <- scores(dca_result$evals.decorana)
# CALCULATE THE SUM OF ALL EIGENVALUES
total_eigenvalues <- sum(dca_eigenvalues)
# CALCULATE THE PERCENTAGES OF EACH EIGENVALUE (round to 2nd decimal)
dca_percent <- round((dca_eigenvalues / total_eigenvalues) * 100, 2)

## CHECK CLUSTER----------------------------------------------------------------
if (clusterized) {
  # print("CA was performed!")
  ca_clust_vector <- ca_vector
  ca_colors <- ca_cmap[1:max(ca_vector)]
} else {
  # print("CA was NOT performed!")
  row_names <- rownames(dca_scores)
  ca_clust_vector <- rep(1, length(row_names))
  ca_colors <- ca_cmap[1]
}

## PLOTS------------------------------------------------------------------------
if (length(dcs_to_plot) == 2) { # Minimum 2 dcs
  # print("Only 2 dcs selected to biplot")
  # PREPARE SAMPLE df___________________________________________________________
  samples_df <- data.frame(
    x = dca_scores[,dcs_to_plot[1]],
    y = dca_scores[,dcs_to_plot[2]],
    cluster = ca_clust_vector,
    label = rownames(dca_scores),
    stringsAsFactors = FALSE
  )
  # Eigenvectors df_____________________________________________________________
  eigenvectors_df <- data.frame(
    x = dca_eigenvectors[, dcs_to_plot[1]],
    y = dca_eigenvectors[, dcs_to_plot[2]],
    label = rownames(dca_eigenvectors),
    stringsAsFactors = FALSE
  )
  # PLOT PC_n°1 vs PC_n°2 ______________________________________________________
  dca_biplot_1 <- ggplot() +
      geom_point(data = samples_df, aes(x = x, y = y, 
                                        color = factor(ca_clust_vector), 
                                        fill = factor(ca_clust_vector)),
                 alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
      # geom_text(data = samples_df, aes(x = x, y = y, label = label),
      #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
      xlab(paste0(dcs_to_plot[1], " (", dca_percent[dcs_to_plot[1],], "%)")) +
      ylab(paste0(dcs_to_plot[2], " (", dca_percent[dcs_to_plot[2],], "%)")) +
      geom_segment(data = eigenvectors_df,
                 aes(x = dca_result$origin[dcs_to_plot[1]], 
                     y = dca_result$origin[dcs_to_plot[2]], 
                     xend = x * m_factor + dca_result$origin[dcs_to_plot[1]], 
                     yend = y * m_factor + dca_result$origin[dcs_to_plot[2]]),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "black", linewidth = 0.5) +
      # Eigenvectors labels
      geom_text(data = data.frame(x = dca_eigenvectors[, 1] * m_factor + dca_result$origin[dcs_to_plot[1]], 
                                  y = dca_eigenvectors[, 2] * m_factor + dca_result$origin[dcs_to_plot[2]], 
                                  rownames(dca_eigenvectors)),
                aes(x = x, y = y, label = rownames(dca_eigenvectors)), size = 4, hjust = 0, vjust = 1, color = "blue") +
      # Color mapping
      scale_color_manual(name = "Clusters", values = ca_colors) +
      scale_fill_manual(name = "Clusters", values = ca_colors) +
      theme_minimal() 
  
  # RETURN only PC_n°1 vs PC_n°2 _______________________________________________
  outputs <- list(dca_biplot_1)
  
}else if (length(dcs_to_plot) == 3) { # Maximum 3 dcs
  # print("3 dcs selected to biplot")
  # PREPARE SAMPLE df___________________________________________________________
  samples_df <- data.frame(
    x = dca_scores[, dcs_to_plot[1]],
    y = dca_scores[, dcs_to_plot[2]],
    z = dca_scores[, dcs_to_plot[3]],
    cluster = ca_clust_vector,
    label = rownames(dca_scores),
    stringsAsFactors = FALSE
  )
  # Eigenvectors df_____________________________________________________________
  eigenvectors_df <- data.frame(
    x = dca_eigenvectors[, dcs_to_plot[1]],
    y = dca_eigenvectors[, dcs_to_plot[2]],
    z = dca_eigenvectors[, dcs_to_plot[3]],
    label = rownames(dca_eigenvectors)
  )
  
  # PLOT PC_n°1 vs PC_n°2 ______________________________________________________
  dca_biplot_1 <- ggplot() +
      geom_point(data = samples_df, aes(x = x, y = y, 
                                        color = factor(ca_clust_vector), 
                                        fill = factor(ca_clust_vector)),
                 alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
      # geom_text(data = samples_df, aes(x = x, y = y, label = label),
      #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
      xlab(paste0(dcs_to_plot[1], " (", dca_percent[dcs_to_plot[1],], "%)")) +
      ylab(paste0(dcs_to_plot[2], " (", dca_percent[dcs_to_plot[2],], "%)")) +
      # Eigenvectors arrows
      geom_segment(data = eigenvectors_df,
                   aes(x = dca_result$origin[dcs_to_plot[1]], 
                       y = dca_result$origin[dcs_to_plot[2]], 
                       xend = x * m_factor + dca_result$origin[dcs_to_plot[1]], 
                       yend = y * m_factor + dca_result$origin[dcs_to_plot[2]]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "black", linewidth = 0.5) +
      # Eigenvectors labels
      geom_text(data = data.frame(x = dca_eigenvectors[, 1] * m_factor + dca_result$origin[dcs_to_plot[1]], 
                                  y = dca_eigenvectors[, 2] * m_factor + dca_result$origin[dcs_to_plot[2]], 
                                  rownames(dca_eigenvectors)),
                aes(x = x, y = y, label = rownames(dca_eigenvectors)), size = 4, hjust = 0, vjust = 1, color = "blue") +
      # Color mapping
      scale_color_manual(name = "Clusters", values = ca_colors) +
      scale_fill_manual(name = "Clusters", values = ca_colors) +
      theme_minimal() 
  
  # PLOT PC_n°1 vs PC_n°3_______________________________________________________
  dca_biplot_2 <- ggplot() +
      geom_point(data = samples_df, aes(x = x, y = z,
                                        color = factor(ca_clust_vector),
                                        fill = factor(ca_clust_vector)),
                 alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
      # geom_text(data = samples_df, aes(x = x, y = z, label = label),
      #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
      xlab(paste0(dcs_to_plot[1], " (", dca_percent[dcs_to_plot[1],], "%)")) +
      ylab(paste0(dcs_to_plot[3], " (", dca_percent[dcs_to_plot[3],], "%)")) +
      # Eigenvectors arrows
      geom_segment(data = eigenvectors_df,
                   aes(x = dca_result$origin[dcs_to_plot[1]],
                       y = dca_result$origin[dcs_to_plot[3]],
                       xend = x * m_factor + dca_result$origin[dcs_to_plot[1]],
                       yend = z * m_factor + dca_result$origin[dcs_to_plot[3]]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "black", linewidth = 0.5) +
      # Eigenvectors labels
      geom_text(data = data.frame(x = (m_factor) * dca_eigenvectors[, 1] + dca_result$origin[dcs_to_plot[1]],
                                  y = (m_factor) * dca_eigenvectors[, 3] + dca_result$origin[dcs_to_plot[3]],
                                label = rownames(dca_eigenvectors)),
              aes(x = x, y = y, label = label), size = 4, hjust = 0, vjust = 1, color = "blue") +
      # Color mapping
      scale_color_manual(name = "Clusters", values = ca_colors) +
      scale_fill_manual(name = "Clusters", values = ca_colors) +
      theme_minimal()
  # dca_biplot_2

  # PLOT PC_n°2 vs PC_n°3_______________________________________________________
  dca_biplot_3 <- ggplot() +
      geom_point(data = samples_df, aes(x = y, y = z,
                                        color = factor(ca_clust_vector),
                                        fill = factor(ca_clust_vector)),
                 alpha = 0.9, size = 4, shape = 21, stroke = 0.25) +
      # geom_text(data = samples_df, aes(x = y, y = z, label = label),
      #           size = 4, hjust = -0.2, vjust = 0.5, colour = "black") +
      xlab(paste0(dcs_to_plot[2], " (", dca_percent[dcs_to_plot[2],], "%)")) +
      ylab(paste0(dcs_to_plot[3], " (", dca_percent[dcs_to_plot[3],], "%)")) +
      # Eigenvectors arrows
      geom_segment(data = eigenvectors_df,
                   aes(x = dca_result$origin[dcs_to_plot[2]], 
                       y = dca_result$origin[dcs_to_plot[3]], 
                       xend = y * m_factor + dca_result$origin[dcs_to_plot[2]], 
                       yend = z * m_factor + dca_result$origin[dcs_to_plot[3]]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "black", linewidth = 0.5) +
      # Eigenvectors labels
      geom_text(data = data.frame(x = dca_eigenvectors[,2] * m_factor + dca_result$origin[dcs_to_plot[2]],
                                  y = dca_eigenvectors[,3] * m_factor + dca_result$origin[dcs_to_plot[3]],
                                  rownames(dca_eigenvectors)),
                aes(x = x, 
                    y = y, 
                    label = rownames(dca_eigenvectors)),
                size = 4, hjust = 0, vjust = 1, color = "blue") +
      # Color mapping
      scale_color_manual(name = "Clusters", values = ca_colors) +
      scale_fill_manual(name = "Clusters", values = ca_colors) +
      theme_minimal()

  # PLOT 3D ____________________________________________________________________
  # Create a DCA scatter plot
  dca_plot_3d <- plot_ly()
  # Add scatterplot for sample data
  dca_plot_3d <- dca_plot_3d %>%
    add_trace(data = samples_df, type = "scatter3d", mode = "markers", 
              x = ~x, y = ~y, z = ~z, 
              color = ~ca_clust_vector, colors = ca_colors,
              marker = list(size = 8, opacity = 0.8, 
                            line = list(width = 1, color = "DarkSlateGrey")), 
              name = "Sample data", showlegend = TRUE)
  # # Add labels for sample data
  # dca_plot_3d <- dca_plot_3d %>%
  #   add_trace(data = samples_df, type = "scatter3d", mode = "text", 
  #             x = ~x, y = ~y, z = ~z, text = ~label,
  #             textfont = list(size = 12, color = "black"), 
  #             name = "Id labels", showlegend = TRUE)
  # Add arrows
  for (i in seq_len(nrow(eigenvectors_df))) {
    dca_plot_3d <- dca_plot_3d %>%
      add_trace(type = "scatter3d", mode = "lines",
                x = c(dca_result$origin[dcs_to_plot[1]], 
                      eigenvectors_df$x[i] * m_factor + dca_result$origin[dcs_to_plot[1]]),  
                y = c(dca_result$origin[dcs_to_plot[2]], 
                      eigenvectors_df$y[i] * m_factor + dca_result$origin[dcs_to_plot[2]]),
                z = c(dca_result$origin[dcs_to_plot[3]], 
                      eigenvectors_df$z[i] * m_factor + dca_result$origin[dcs_to_plot[3]]),
                line = list(color = "black", width = 2),
                name = paste("Arrow", i),
                showlegend = FALSE) 
  }
  # Add labels for sample data
  dca_plot_3d <- dca_plot_3d %>%
    add_trace(data = eigenvectors_df, type = "scatter3d", mode = "text", 
              x = ~x * m_factor + dca_result$origin[dcs_to_plot[1]], 
              y = ~y * m_factor + dca_result$origin[dcs_to_plot[2]], 
              z = ~z * m_factor + dca_result$origin[dcs_to_plot[3]],
              text = ~label,
              textfont = list(size = 12, color = "blue"), 
              name = "Eigen vector names",
              showlegend = FALSE)
  # Layout
  dca_plot_3d <- dca_plot_3d %>%
    layout(
      scene = list(
        xaxis = list(title = paste0(dcs_to_plot[1], " (", dca_percent[dcs_to_plot[1],], "%)")),
        yaxis = list(title = paste0(dcs_to_plot[2], " (", dca_percent[dcs_to_plot[2],], "%)")),
        zaxis = list(title = paste0(dcs_to_plot[3], " (", dca_percent[dcs_to_plot[3],], "%)")),
        margin = list(l = 0, r = 0, b = 0, t = 0), showlegend = TRUE, legend = list(x = 0, y = 1)
      ))

  # Save 1vs2 + 1vs3 + 2vs3 + 3D in list----------------------------------------
  outputs <- list(dca_biplot_1, dca_biplot_2, dca_biplot_3, dca_plot_3d)
}
# RETURN========================================================================
outputs

