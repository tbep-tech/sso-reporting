library(dplyr)
library(stringdist)
library(tidytext)
library(stringr)
library(ggplot2)

load(file = here::here("data/vols.RData"))

facilities <- vols %>% 
  summarise(
    cnt = n(),
    .by = facility
  )

# Function to create a simplified version of facility name for comparison
simplify_name <- function(name) {
  name %>%
    # Convert to lowercase
    tolower() %>%
    # Remove numbers
    gsub("[0-9]", "", .) %>%
    # Remove common words and abbreviations
    gsub("\\b(city|of|the|and|a|an)\\'?\\b", "", .) %>%
    # Remove special characters
    gsub("[[:punct:]]", " ", .) %>%
    # Remove extra whitespace
    str_squish()
}

# Create simplified names
facilities_simplified <- facilities %>%
  mutate(
    simplified_name = simplify_name(facility),
    name_length = nchar(facility)
  )

# Create a distance matrix based on string similarity
name_dist_matrix <- stringdistmatrix(
  facilities_simplified$simplified_name, 
  method = "jw"  # Jaro-Winkler distance
)

# Cluster similar names
# Lower threshold means more aggressive clustering
cluster_threshold <- 0.3
name_clusters <- hclust(as.dist(name_dist_matrix))
facilities_simplified$cluster <- cutree(name_clusters, h = cluster_threshold)

# Analyze clusters
cluster_analysis <- facilities_simplified %>%
  group_by(cluster) %>%
  summarise(
    cluster_size = n(),
    unique_names = n_distinct(facility),
    representative_name = facility[which.max(name_length)],
    all_names = paste(unique(facility), collapse = " | ")
  ) %>%
  filter(cluster_size > 1)  # Only show clusters with multiple entries

# join representative names to vols - not perfec
tojn <- cluster_analysis %>%
  select(representative_name, all_names) %>%
  unnest_tokens(facility, all_names, token = "regex", pattern = " \\| ", to_lower = F)
vols <- vols %>%
  left_join(tojn, by = 'facility')


ggplot(vols, aes(x = volest)) +
  stat_ecdf(geom = "step", color = "blue") +
  scale_x_log10() +  # Set x-axis to log scale
  facet_wrap(~yr) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor.x = element_blank()
  )

toplo <- vols |> 
  st_set_geometry(NULL) |> 
  filter(yr %in% 2024) |>
  filter(volest > 1e5) |>
  summarise(
    volest = sum(volest),
    .by = c(yr, representative_name)
  )

# show a bar plot of volumes by facility
# represenative_name on y-axis, ordered by volest
ggplot(toplo, aes(x = volest / 1e6, y = reorder(representative_name, volest))) +
  geom_col() +
  # facet_wrap(~yr) +
  labs(
    x = "Volume (mill gallons)",
    y = "Facility",
    title = "Top Facilities by Volume in 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
