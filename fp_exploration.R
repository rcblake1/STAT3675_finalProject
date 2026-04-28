library(factoextra)

data("state")
df <- as.data.frame(state.x77)
head(df)


# Load necessary library for visualization


# Define the exact variables to keep
vars_to_keep <- c("Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area")

# Subset the dataframe using base R indexing [rows, columns]
df_subset <- df[, vars_to_keep]

# Scale the data 
df_scaled <- scale(df_subset)



set.seed(428)
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(title = "Optimal Number of Clusters", subtitle = "Elbow Method")

km_res <- kmeans(df_scaled, centers = 5, nstart = 25)

# Append the cluster assignments as a new column using the $ operator
df_subset$Cluster <- as.factor(km_res$cluster)

# Visualize the clusters on a 2D plane
fviz_cluster(km_res, data = df_scaled,
             palette = "jco",
             star.plot = TRUE, 
             repel = TRUE,     
             ggtheme = theme_minimal(),
             main = "State Clusters based on Demographics & Geography")

# Calculate the mean of every variable, grouped by the new Cluster column
cluster_profiles <- aggregate(. ~ Cluster, data = df_subset, FUN = mean)

# View the profiles to interpret what each cluster represents
print(cluster_profiles)

# Print just the state names (which are the row names) and their cluster assignment
df_sorted <- df_subset[order(df_subset$Cluster), ]
print(df_sorted[, "Cluster", drop = FALSE])