library(dplyr)
library(tidyr)
library(tidytext)
library(zoo)
library(ggplot2)
library(maps)
library(readr)
library(purrr)  # For map2

# Load preprocessed data
data <- read_csv("C:/Users/ashri/Downloads/Submissionf/Submissionf/data/preprocessed_sentiment_data.csv")

# Temporal Analysis: Daily and Weekly Sentiment Trends (Research Question 1)
daily_sentiment <- data %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date, politician) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

# Weekly sentiment (7-day rolling mean)
weekly_sentiment <- daily_sentiment %>%
  group_by(politician) %>%
  arrange(date) %>%
  mutate(weekly_avg_sentiment = rollmean(avg_sentiment, k = 7, fill = NA, align = "right"))

# Define major political events (hypothetical for 2025)
events <- data.frame(
  date = as.Date(c("2025-03-01", "2025-04-01")),
  event = c("Trump Tariff Announcement", "Midterm Election Debate")
)

# Plot daily and weekly sentiment with event markers
p1 <- ggplot() +
  geom_line(data = daily_sentiment, aes(x = date, y = avg_sentiment, color = politician, linetype = "Daily")) +
  geom_line(data = weekly_sentiment, aes(x = date, y = weekly_avg_sentiment, color = politician, linetype = "Weekly"), linewidth = 1.2) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "gray50") +
  geom_text(data = events, aes(x = date, y = 20, label = event), angle = 90, vjust = -0.5, hjust = 1, size = 3) +
  labs(
    title = "Daily and Weekly Sentiment Trends by Politician",
    subtitle = "With Major Political Events",
    x = "Date", y = "Average Sentiment Score",
    linetype = "Trend Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Platform-Specific Sentiment (Research Question 2)
platform_sentiment <- data %>%
  group_by(platform, politician) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

# Debug: Check number of posts per platform for each politician
platform_counts <- data %>%
  group_by(politician, platform) %>%
  summarize(n_posts = n(), .groups = "drop") %>%
  pivot_wider(names_from = platform, values_from = n_posts, values_fill = 0)

cat("Number of posts per platform for each politician:\n")
print(platform_counts)

# Statistical test for platform differences
platform_test <- data %>%
  group_by(politician) %>%
  summarize(
    twitter_scores = list(sentiment_score[platform == "Twitter"]),
    reddit_scores = list(sentiment_score[platform == "Reddit"]),
    .groups = "drop"
  ) %>%
  mutate(t_test = map2(twitter_scores, reddit_scores, ~ {
    if (length(.x) >= 2 && length(.y) >= 2 && length(unique(.x)) > 1 && length(unique(.y)) > 1) {
      t.test(.x, .y)$p.value
    } else {
      NA_real_
    }
  })) %>%
  unnest(t_test) %>%
  filter(!is.na(t_test))

cat("T-test p-values for platform sentiment differences by politician:\n")
print(platform_test)

p2 <- ggplot(platform_sentiment, aes(x = platform, y = avg_sentiment, fill = politician)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment by Platform",
       subtitle = "Twitter: Younger/Urban, Reddit: Mixed Demographics",
       x = "Platform", y = "Average Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Geographic Analysis (Research Question 3)
# Standardize location to US states where possible
us_states <- tolower(state.name)  # List of US states
data_geo <- data %>%
  mutate(location_clean = case_when(
    grepl("GA|Georgia", location, ignore.case = TRUE) ~ "Georgia",
    grepl("NY|New York", location, ignore.case = TRUE) ~ "New York",
    grepl("CA|California", location, ignore.case = TRUE) ~ "California",
    # Add more mappings as needed
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(location_clean))

geo_sentiment <- data_geo %>%
  group_by(location_clean, politician) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop") %>%
  rename(state = location_clean) %>%
  mutate(state = tolower(state))
library(ggplot2)
library(dplyr)
library(maps)

# Load and prepare map data
us_map <- map_data("state")

# Make sure geo_sentiment has one row per state
geo_sentiment <- geo_sentiment %>%
  mutate(state = tolower(state)) %>%
  group_by(state) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE))  # or use your column

# Merge with map data
geo_plot_data <- us_map %>%
  left_join(geo_sentiment, by = c("region" = "state"))

# Plot US map with sentiment scores
ggplot(geo_plot_data, aes(x = long, y = lat, group = group, fill = avg_sentiment)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Avg Sentiment"
  ) +
  labs(title = "Average Sentiment by State", x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())


# Map plot
p3 <- ggplot(geo_plot_data, aes(x = long, y = lat, group = group, fill = avg_sentiment)) +
  geom_polygon(color = "white", size = 0.1) +
  facet_wrap(~ politician) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "gray90") +
  labs(title = "Sentiment by US State", fill = "Average Sentiment Score") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())

# Demographic Analysis (Proxy via Platform)
demo_sentiment <- data %>%
  mutate(demographic = case_when(
    platform == "Twitter" ~ "Younger/Urban",
    platform == "Reddit" ~ "Mixed",
    TRUE ~ "Unknown"
  )) %>%
  group_by(demographic, politician) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(demo_sentiment, aes(x = demographic, y = avg_sentiment, fill = politician)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment by Demographic Proxy (via Platform)",
       x = "Demographic Segment", y = "Average Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Topic-Specific Sentiment (Research Question 1)
topic_sentiment <- data %>%
  group_by(topic, politician) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(topic_sentiment, aes(x = topic, y = avg_sentiment, fill = politician)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment by Topic", x = "Topic", y = "Average Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Comparison with Polling Data (Research Question 4)
# Simulated polling data for 2025 (example for Donald Trump)
polling_data <- data.frame(
  date = seq(as.Date("2025-01-01"), as.Date("2025-04-20"), by = "week"),
  politician = "Donald Trump",
  approval_rating = c(45, 44, 46, 43, 42, 41, 40, 45, 48, 50, 47, 46, 44, 43, 42, 41)
)

# Merge sentiment and polling data
sentiment_polling <- daily_sentiment %>%
  filter(politician == "Donald Trump") %>%
  left_join(polling_data, by = c("date", "politician"))

# Compute correlation
correlation <- cor.test(sentiment_polling$avg_sentiment, sentiment_polling$approval_rating, use = "complete.obs")
cat("Correlation between sentiment and polling approval ratings for Donald Trump:\n")
cat("Correlation coefficient:", correlation$estimate, "\n")
cat("P-value:", correlation$p.value, "\n")

# Plot sentiment vs polling
p6 <- ggplot(sentiment_polling, aes(x = date)) +
  geom_line(aes(y = avg_sentiment, color = "Sentiment Score")) +
  geom_line(aes(y = approval_rating / 5 - 10, color = "Approval Rating (Scaled)")) +  # Scale approval for visualization
  labs(title = "Sentiment vs Polling Approval Ratings (Donald Trump)",
       x = "Date", y = "Sentiment Score / Scaled Approval Rating",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plots
dir.create("plots", showWarnings = FALSE)
ggsave("plots/daily_sentiment.png", p1, width = 10, height = 6)
ggsave("plots/platform_sentiment.png", p2, width = 8, height = 6)
ggsave("plots/geo_sentiment.png", p3, width = 10, height = 6)
ggsave("plots/demo_sentiment.png", p4, width = 8, height = 6)
ggsave("plots/topic_sentiment.png", p5, width = 8, height = 6)
ggsave("plots/sentiment_vs_polling.png", p6, width = 8, height = 6)

# Summary statistics
summary_stats <- data %>%
  group_by(politician, sentiment_category) %>%
  summarize(count = n(), avg_score = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

write_csv(summary_stats, "C:/Users/ashri/Downloads/Submissionf/Submissionf/data/sentiment_summary.csv")