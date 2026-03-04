# ======================================================
# Step 4: Clustering Analysis
# ======================================================

library(tidyverse)
library(janitor)

# Load data if needed
if (!exists("df")) {
  df <- readr::read_csv("Data/World_Happiness_Report_2005_2022.csv",
                        show_col_types = FALSE) %>%
    clean_names()
}

# Select variables for clustering
clust_df <- df %>%
  select(
    log_gdp_per_capita,
    social_support,
    healthy_life_expectancy_at_birth,
    freedom_to_make_life_choices,
    perceptions_of_corruption,
    positive_affect,
    negative_affect
  ) %>%
  drop_na()

# Scale data
clust_scaled <- scale(clust_df)

# 1) Elbow method
set.seed(123)
wss <- map_dbl(1:10, function(k) {
  kmeans(clust_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- tibble(
  k = 1:10,
  wss = wss
)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Method for K-means Clustering",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  )

# 2) K-means with k = 3
set.seed(123)
km3 <- kmeans(clust_scaled, centers = 3, nstart = 25, iter.max = 50)

clust_results <- clust_df %>%
  mutate(cluster = factor(km3$cluster))

# Save clustering results
write_csv(clust_results, "outputs/clustering_results_k3.csv")

# Quick cluster summary
clust_summary <- clust_results %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

print(clust_summary)