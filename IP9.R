# 📚 Load required libraries
install.packages(c("arules", "tidyverse", "Matrix", "arulesViz", "igraph", "viridis"))

library(arules)
library(tidyverse)
library(Matrix)
library(arulesViz)
library(igraph)
library(viridis)

# 📂 Define file paths
file_path_main <- "C:/Users/stefa/OneDrive - Careered - CTU/2025/CS871/Week 9/lastfm-dataset-360K/lastfm-dataset-360K/usersha1-artmbid-artname-plays.tsv"
file_path_profile <- "C:/Users/stefa/OneDrive - Careered - CTU/2025/CS871/Week 9/lastfm-dataset-360K/lastfm-dataset-360K/usersha1-profile.tsv"

# 📥 Load datasets
data_main <- read.delim(file_path_main, header = FALSE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)
data_profile <- read.delim(file_path_profile, header = FALSE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)

# 🏷 Rename columns
colnames(data_main) <- c("user_id", "artist_id", "artist_name", "plays")
colnames(data_profile) <- c("user_id", "gender", "age", "country", "signup_date")

# 🔍 View dataset structure
str(data_main)
str(data_profile)

# 🔥 View summary statistics
summary(data_main)
summary(data_profile)

# 📌 View first few rows
head(data_main)
head(data_profile)

# 🧐 Check for missing values
sum(is.na(data_main))
sum(is.na(data_profile))

# 🗑 Remove missing values if necessary
data_main <- na.omit(data_main)
data_profile <- na.omit(data_profile)

# 🔁 Remove duplicate entries
data_main <- distinct(data_main)
data_profile <- distinct(data_profile)

# 🎯 Keep only relevant columns
data_main <- select(data_main, user_id, artist_name, plays)

# 📊 Number of unique users and artists
num_users <- length(unique(data_main$user_id))
num_artists <- length(unique(data_main$artist_name))

cat("Total Users:", num_users, "\n")
cat("Total Artists:", num_artists, "\n")

# 🔥 Top 10 most played artists
top_artists <- data_main %>%
  group_by(artist_name) %>%
  summarise(total_plays = sum(plays)) %>%
  arrange(desc(total_plays)) %>%
  slice_head(n = 10)

print(top_artists)

# 📊 Histogram of user activity (plays)
ggplot(data_main, aes(x = plays)) +
  geom_histogram(bins = 50, fill = "slateblue", color = "black") +
  scale_x_log10() +
  ggtitle("Distribution of Play Counts") +
  xlab("Number of Plays (log scale)") +
  ylab("Count of Users")

# 🎵 Select the top 104 artists
top_104_artists <- data_main %>%
  group_by(artist_name) %>%
  summarise(total_plays = sum(plays)) %>%
  arrange(desc(total_plays)) %>%
  slice_head(n = 104) %>%
  pull(artist_name)

# 🎛 Filter dataset for only these top artists
filtered_data <- data_main %>%
  filter(artist_name %in% top_104_artists) %>%
  mutate(value = 1) %>%
  select(user_id, artist_name, value)

# 📌 Convert data to a user-item matrix
user_artist_matrix <- filtered_data %>%
  pivot_wider(names_from = artist_name, values_from = value, values_fill = list(value = 0)) %>%
  select(-user_id) %>%
  as.matrix()

# 🎭 Ensure only 0s and 1s
user_artist_matrix[user_artist_matrix > 1] <- 1  

# 🔄 Convert to sparse matrix format
trans <- as(user_artist_matrix, "transactions")

# 🏆 Display summary of transactions
summary(trans)

# 🏆 Apply Apriori Algorithm
rules <- apriori(trans, parameter = list(supp = 0.005, conf = 0.5, minlen = 2))

# 🔍 View top 10 rules sorted by lift
inspect(sort(rules, by = "lift")[1:10])

# 🎯 Select top 50 rules for visualization
subrules_50 <- head(sort(rules, by = "lift"), 50)

# 🎵 Convert rules into an igraph object
rules_graph <- plot(subrules_50, method = "graph", engine = "igraph", plot = FALSE)

# 🎨 **Plot Cluster Graph**
plot(rules_graph,
     layout = layout_with_fr(rules_graph),
     vertex.size = 5,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     edge.arrow.size = 0.25,
     margin = 0)

rules_large <- apriori(trans, parameter = list(supp = 0.003, conf = 0.6, minlen = 2, maxlen = 5))
scatterplot_rules <- subset(rules_large, lift >= 3 & confidence >= 0.7)

plot(scatterplot_rules, method = "scatterplot", measure = c("support", "lift"), shading = "confidence")

plot(subrules_50, method = "matrix", measure = "lift", 
     control = list(reorder = "support/confidence", col = viridis::mako(2.5)))

plot(subrules_50, method = "grouped", control = list(col = hcl.colors(5, "Blues")))

