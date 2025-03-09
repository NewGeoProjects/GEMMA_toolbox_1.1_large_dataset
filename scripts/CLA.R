

##========================INPUTS from Shiny app=================================
# Dataset
env_molecules <- selected_molecules
# Scale and center variables
env_molecules <- decostand(env_molecules, method = "standardize")

# Read selector
k_selec <- k_selector

# Create cmap for clusters
ca_cmap <- c("red", "purple", "orange", "brown", "green", "blue", "pink", 
             "brown", "gray", "lightblue", "darkred", "darkorange", "darkyellow", 
             "darkgreen", "darkblue", "darkpurple", "darkpink", "darkbrown", 
             "darkgray", "magenta")

  
# Read k_values_result
if (k_selec == 4){
  k_values_result <- k_values
}else{
  k_values_result <- k_values()
}

# Convert k_values_result to numeric if necessary
if (is.numeric(k_values_result)) {
  print("k_values_result is already numeric")
} else {
  k_values_numeric <- as.numeric(k_values_result)
}


# Select the k_value_result
if (k_selec == 1){
  n_Clusters <- k_values_result[[1]]
}else if(k_selec == 2){
  n_Clusters <- k_values_result[[2]]
}else if(k_selec == 3){
  n_Clusters <- k_values_result[[3]]
}else if(k_selec == 4){
  n_Clusters <- k_values_result
}

if(n_Clusters > 1){ ## Check cluster number > 1
  # CA method
  clust_method <- sbox_ca_method
  # Scale and center variables--------------------------------------------------
  env_molecules <- decostand(env_molecules, method = "standardize")
  
  ##========================= CLUSTER ANALYSIS =================================
  if (clust_method == 1){ # OPTION 1: HIERARCHICAL -----------------------------
    # Enhanced hierarchical clustering
    # res.hc <- eclust(env_molecules, "hclust", k = n_Clusters)
    # print(res.hc)
    
    #################################################################################
    
    
    ## generate data
    data <- env_molecules
    k = n_Clusters
    ## pick colour
    cols = ca_cmap[1:n_Clusters]
    
    ## perform hierarchical clustering 
    d_data <- dist(data)
    hc_data <- hclust(d_data, method = "complete")
    
    ## cut tree to three clusters
    clust_data <- cutree(hc_data,k=k)
    
    
    ## plot dendrogram
    clust_main_plot = fviz_dend(hc_data, k_colors = cols, k=k) +
      theme(panel.background = element_blank())
    
    ## create silhouette data
    sil_data = as.numeric(clust_data)
    names(sil_data) = rownames(data)
    sil_data = silhouette(sil_data, d_data)
    
    ## extract dendrogram from ggplot object and create named color vector
    dend = attributes(clust_main_plot)$dendrogram
    tree_order <- order.dendrogram(dend)
    clust_data = factor(clust_data, levels = unique(clust_data[tree_order]))
    names(cols) = unique(clust_data[tree_order])
    
    ## plot silhouettes
    clust_sil_plot <- fviz_silhouette(sil_data, print.summary = FALSE, title="Silhouette of HC") + 
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      theme(panel.background = element_blank())
      
    # ## show as grid
    # plot_grid(clust_main_plot, clust_sil_plot)
    
    # Output
    cluster_out <- as.numeric(clust_data)

    
  } else if (clust_method == 2){ # OPTION 2: ONLY K-MEANS ----------------------
    # K-means clustering
    km.res <- eclust(env_molecules, "kmeans", k = n_Clusters)
    # k-means group number of each observation
    head(km.res$cluster)
    # k-means clusters plot
    clust_main_plot <- fviz_cluster(km.res,  frame.type = "norm", frame.level = 0.68, title="KMEANS CLUSTER", palette = ca_cmap[1:n_Clusters])
    # Silhuette plot
    clust_sil_plot <- fviz_silhouette(km.res, title="Silhouette of HC", palette = ca_cmap[1:n_Clusters])
    # Output
    cluster_out <- km.res$cluster
    
  } else if (clust_method == 3){ # OPTION 3: HC CLUST AND THEN KMEANS-------------
    # HC clustering with "ward.D2" method on euclidean distances of "env_molecules" dataset
    euc_dis = dist(env_molecules, method = "euclidean")
    res.hc = hclust(euc_dis,  method = "ward.D2")
    # Subdivide the HC output into k clusters selected by (WSS, GAP, SIL, user's choice)
    grp <- cutree(res.hc, k = n_Clusters)
    # Aggregate the "env_molecules" dataset according to groups selected
    clus.centers <- aggregate(env_molecules, list(grp), mean)[, -1]
    # Perform K-MEANS on "env_molecules"  dataset using the centers calculated
    res.km <- kmeans(env_molecules, centers = clus.centers, iter.max = 10, algorithm = "Hartigan-Wong")
    # print(res.km$cluster[res.hc$order])
    
    # Prepare color vector for lables
    lab_array <- res.km$cluster[res.hc$order]
    my_labels <- ca_cmap[lab_array]
    
    # Dendogram plot
    clust_main_plot <- fviz_dend(res.hc, main ="HKMEANS CLUSTER", k = n_Clusters, 
                                 k_colors = "black",
                                 # k_colors = ca_cmap[1:n_Clusters],
                                 rect = TRUE, 
                                 label_cols = my_labels,
                                 color_labels_by_k = FALSE,
                                 cex = 0.6)
    # Silhuette plot
    sil_widths <- silhouette(res.km$cluster, dist(env_molecules)) # Calculate silhouette widths for the K-means clusters
    clust_sil_plot <- fviz_silhouette(sil_widths, title="Silhouette of HC", palette = ca_cmap[1:n_Clusters]) # Create a silhouette plot
    # Output
    cluster_out <- res.km$cluster
  }
  
  # Return the ggplot object ===================================================
  list(clust_main_plot, clust_sil_plot, cluster_out)
}




