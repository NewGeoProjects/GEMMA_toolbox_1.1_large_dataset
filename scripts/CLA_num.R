

## SETUP =======================================================================
# Parameters____________________________________________________________________
# ALL
clust_num_max <- 20
threshold_k <- 2
# WSS
threshold_wss <- 1
# GAP
B_gap <- gap_b
nstart_gap <- gap_nstart

## Import from app==============================================================
ca_method <- fun_method
env_molecules <- selected_molecules

# Scale and center variables
env_molecules <- decostand(env_molecules, method = "standardize")

# PERFORM K EVALUATION (WSS, GAP, SILHOUETTE) ==================================
if (ca_method == 1){
  print("Hierarchical method selected")
  wss_fviz <- fviz_nbclust(env_molecules, FUN = hcut, method = "wss", k.max = clust_num_max)
  gap_fviz <- clusGap(env_molecules, FUN = hcut, K.max = clust_num_max, B = B_gap, nstart = nstart_gap)
  sil_fviz <- fviz_nbclust(env_molecules, FUN = hcut, method = "silhouette", k.max = clust_num_max)
} else { 
  print("k-means or hkmeans selected")
  wss_fviz <- fviz_nbclust(env_molecules, FUN = kmeans, method = "wss", k.max = clust_num_max)
  gap_fviz <- clusGap(env_molecules, FUN = kmeans, K.max = clust_num_max, B = B_gap, nstart = nstart_gap)
  sil_fviz <- fviz_nbclust(env_molecules, FUN = kmeans, method = "silhouette", k.max = clust_num_max)
}

## K EVALUATION ================================================================
# 1) WSS________________________________________________________________________
"
Find the optimal number of clusters based on the WSS. 
The first inflection point of the second derivateve (d'') of the WSS corresponds
to the minimum of the best significative cluster number (k)
"
# Extract WSS vector data
wss <- as.numeric(wss_fviz$data$y)
# Determine the optimal number of Clusters from d'' of wss (Elbow point)
wss_k <- which(diff(diff(wss[threshold_k:length(wss)])) < threshold_wss)[1]
# Create PLOT
wss_plot <- wss_fviz + 
  geom_vline(xintercept = wss_k, linetype = "dashed", color = "steelblue") +
  theme_minimal() + 
  ggtitle("WSS METHOD")
# wss_plot

# 2) GAP________________________________________________________________________
"
Find the optimal number of clusters based on the Gap Statistic. 
The larger the gap, the better the number of clusters k fits the data.
"
# Extract the gap and SE values
gap <- gap_fviz$Tab[, "gap"]
se <- gap_fviz$Tab[, "SE.sim"]
# Determine the optimal K
gap_k <- as.numeric(maxSE(gap, se))
# Create PLOT
gap_plot <- fviz_gap_stat(gap_fviz) + 
  theme_minimal() + 
  ggtitle("GAP METHOD")

# 3) SILHOUETTE_________________________________________________________________
"
The optimal number of clusters k is the one that maximize the average silhouette 
over a range of possible values for k.
"
# Find optimal number of cluster
sil_n_clust <- sil_fviz$data
sil_k <- as.numeric(sil_n_clust$clusters[which.max(sil_n_clust$y)])
# Create PLOT
sil_plot <- sil_fviz + 
  theme_minimal() + 
  ggtitle("SILHOUETTE METHOD")

## RETURN OUTPUTS ==============================================================
nclust_list <- list(wss_k, gap_k, sil_k)
list (wss_plot, gap_plot, sil_plot, nclust_list)
