#Set working directory
getwd()
setwd("C:/Users/ashri/Downloads/Submissionf")

# Import required libraries
library(dplyr)
library(tidyr)
library(tidytext)
library(textstem)
library(topicmodels)
library(randomForest)
library(readr)
library(Matrix)

# Load raw data with error handling
tryCatch({
  twitter_data <- read_csv("C:/Users/ashri/Downloads/Submissionf/Submissionf/data/twitter/tweets_20250420_120446.csv")
  reddit_data <- read_csv("C:/Users/ashri/Downloads/Submissionf/Submissionf/data/reddit/reddit_posts_20250420_125024.csv")
}, error = function(e) {
  stop("Error loading CSV files: ", e$message)
})

# Print column names and unique politicians for debugging
cat("Twitter columns:", colnames(twitter_data), "\n")
cat("Reddit columns:", colnames(reddit_data), "\n")
cat("Twitter politicians:", paste(unique(twitter_data$politician), collapse = ", "), "\n")
cat("Reddit politicians:", paste(unique(reddit_data$politician), collapse = ", "), "\n")

# Standardize Twitter data using exact column names
twitter_processed <- twitter_data %>%
  select(status_id, created_at, text, politician, location) %>%
  rename(content_id = status_id, timestamp = created_at) %>%
  mutate(platform = "Twitter",
         timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S+00:00", tz = "UTC"),
         content_id = as.character(content_id))

# Create location lookup from Twitter data
location_lookup <- twitter_processed %>%
  filter(!is.na(location), location != "") %>%
  group_by(politician) %>%
  summarize(locations = list(unique(location))) %>%
  mutate(locations = lapply(locations, function(x) x[!is.na(x)]))

# Print location lookup for debugging
cat("Location lookup:\n")
print(location_lookup)

# Function to assign a random location from the lookup
assign_location <- function(politician, lookup) {
  all_locs <- unlist(lookup$locations)
  if (length(all_locs) == 0) return(NA_character_)
  if (politician %in% lookup$politician) {
    locs <- lookup$locations[lookup$politician == politician][[1]]
    if (length(locs) > 0) {
      return(sample(locs, 1))
    }
  }
  sample(all_locs, 1)
}

# Standardize Reddit data and assign Twitter locations
reddit_processed <- reddit_data %>%
  mutate(content_id = coalesce(comment_id, post_id)) %>%
  select(content_id, timestamp, text, politician) %>%
  mutate(platform = "Reddit",
         timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         content_id = as.character(content_id),
         location = sapply(politician, assign_location, lookup = location_lookup))

# Check for missing required columns
required_cols <- c("content_id", "timestamp", "text", "politician", "location")
for (df in list(twitter_processed, reddit_processed)) {
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in dataset: ", paste(missing_cols, collapse = ", "))
  }
}

# Combine datasets
combined_data <- bind_rows(twitter_processed, reddit_processed)

# Text preprocessing function
preprocess_text <- function(text) {
  if (is.na(text) || nchar(trimws(text)) == 0) return("")
  text <- tolower(text) # Normalize
  text <- gsub("http\\S+|www\\S+|@\\S+|#\\S+", "", text) # Remove URLs, mentions, hashtags
  text <- gsub("[^[:alnum:]]", " ", text) # Keep alphanumeric
  text <- gsub("\\s+", " ", trimws(text)) # Remove extra whitespace
  tokens <- unnest_tokens(tibble(text = text), word, text, token = "words") # Tokenize
  tokens <- tokens %>%
    anti_join(get_stopwords(), by = "word") %>% # Remove stop words
    mutate(word = lemmatize_words(word)) # Lemmatize
  clean_text <- paste(tokens$word, collapse = " ")
  return(clean_text)
}

# Apply preprocessing
combined_data <- combined_data %>%
  mutate(clean_text = sapply(text, preprocess_text))

# Debug: Check for empty clean_text
cat("Rows with empty clean_text:", sum(combined_data$clean_text == ""), "\n")

# Sentiment scoring with AFINN
install.packages("textdata")

afinn <- get_sentiments("afinn")
combined_data <- combined_data %>%
  unnest_tokens(word, clean_text, token = "words") %>%
  left_join(afinn, by = "word") %>%
  group_by(content_id) %>%
  summarize(sentiment_score = sum(value, na.rm = TRUE)) %>%
  right_join(combined_data, by = "content_id") %>%
  mutate(sentiment_score = coalesce(sentiment_score, 0))

# Topic modeling with LDA
dtm <- combined_data %>%
  unnest_tokens(word, clean_text, token = "words") %>%
  count(content_id, word) %>%
  cast_dtm(content_id, word, n)
lda_model <- LDA(dtm, k = 2, control = list(seed = 123))
topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  mutate(topic = paste0("topic_", topic)) %>%
  select(content_id = document, topic)
combined_data <- combined_data %>%
  left_join(topics, by = "content_id") %>%
  mutate(topic = coalesce(topic, "topic_unknown"))

# Sentiment classification with Random Forest
tfidf <- combined_data %>%
  unnest_tokens(word, clean_text, token = "words") %>%
  count(content_id, word) %>%
  bind_tf_idf(word, content_id, n)

# Reduce vocabulary size: keep top 500 words by frequency
top_words <- tfidf %>%
  group_by(word) %>%
  summarize(total_n = sum(n)) %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 500) %>%
  pull(word)



# Debug: Print matrix dimensions and check for NA
cat("TF-IDF rows (content_id):", length(unique(tfidf$content_id)), "\n")
cat("TF-IDF columns (words):", length(unique(tfidf$word)), "\n")
cat("Any NA in content_id:", any(is.na(tfidf$content_id)), "\n")
cat("Any NA in word:", any(is.na(tfidf$word)), "\n")
cat("Any NA in tf_idf:", any(is.na(tfidf$tf_idf)), "\n")

# Create dense matrix using pivot_wider
train_data_df <- tfidf %>%
  select(content_id, word, tf_idf) %>%
  pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) %>%
  as.data.frame()

# Convert to matrix and remove content_id
train_data <- as.matrix(train_data_df[, -1])

# Debug: Verify train_data
cat("Train data class:", class(train_data), "\n")
cat("Train data dimensions:", dim(train_data)[1], "x", dim(train_data)[2], "\n")

# Simulate labels for demo (in practice, use labeled data)
set.seed(123)
labels <- sample(c("positive", "negative", "neutral"), nrow(train_data), replace = TRUE)
rf_model <- randomForest(x = train_data, y = as.factor(labels), ntree = 100)

# Create prediction data frame and join with combined_data
predictions <- data.frame(
  content_id = train_data_df$content_id,
  sentiment_category = predict(rf_model, train_data)
)

combined_data <- combined_data %>%
  left_join(predictions, by = "content_id") %>%
  mutate(sentiment_category = coalesce(sentiment_category, "neutral"))

# Final dataset
final_data <- combined_data %>%
  select(content_id, platform, timestamp, text, clean_text, politician,
         sentiment_score, sentiment_category, topic, location)

# Export preprocessed data
write_csv(final_data, "data/preprocessed_sentiment_data.csv")
cat("Preprocessed data saved to data/preprocessed_sentiment_data.csv\n")