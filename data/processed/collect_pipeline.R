library(rtweet)        # For Twitter API
library(RedditExtractoR) # For Reddit data
library(rvest)         # For web scraping
library(httr)          # For HTTP requests
library(dplyr)         # For data manipulation
library(lubridate)     # For date handling
library(logger)        # For logging
library(readr)         # For CSV writing

# Set up logging
log_appender(appender_file("data_collection.log"))
log_appender(appender_console())
log_threshold(level = "INFO")
log_layout(layout_simple)

# Create data directories if they don't exist
dir.create("data/twitter", recursive = TRUE, showWarnings = FALSE)
dir.create("data/reddit", recursive = TRUE, showWarnings = FALSE)
dir.create("data/news_comments", recursive = TRUE, showWarnings = FALSE)

# Define political figures to track
political_figures <- c(
  "Donald Trump",           # U.S. President
  "Chuck Schumer",          # Senate Majority Leader
  "Mitch McConnell",        # Senate Minority Leader
  "Mike Johnson",           # Speaker of the House
  "Hakeem Jeffries"         # House Minority Leader
)

# Generate search terms (names and Twitter handles)
search_terms <- c(
  political_figures,
  paste0("@", gsub(" ", "", political_figures))
)

# ----------------------
# Twitter Data Collection
# ----------------------
collect_twitter_data <- function() {
  log_info("Starting Twitter data collection")
  
  # Set up Twitter API credentials (replace with your bearer token)
  bearer_token <- "BEARER_TOKEN"
  options(rtweet_bearer = bearer_token)
  
  all_tweets <- list()
  
  # Define date range (Q1 2025)
  # Note: Historical data requires academic access; simulating recent data here
  start_time <- as.POSIXct("2025-01-01", tz = "UTC")
  end_time <- as.POSIXct("2025-03-31", tz = "UTC")
  
  # Search for tweets related to each political figure
  for (figure in political_figures) {
    tryCatch({
      log_info(sprintf("Collecting tweets for: %s", figure))
      
      # rtweet search (recent tweets, simulating historical)
      query <- sprintf('"%s" -is:retweet lang:en', figure)
      tweets <- search_tweets(
        q = query,
        n = 100,  # Adjust based on API limits
        include_rts = FALSE,
        lang = "en",
        token = bearer_token()
      )
      
      if (nrow(tweets) > 0) {
        # Filter tweets within date range (simulated)
        tweets <- tweets %>%
          mutate(created_at = as.POSIXct(created_at, tz = "UTC")) %>%
          filter(created_at >= start_time & created_at <= end_time)
        
        if (nrow(tweets) > 0) {
          tweet_data <- tweets %>%
            mutate(
              status_id = status_id,
              created_at = created_at,
              text = text,
              user_id = user_id,
              location = location,
              retweet_count = retweet_count,
              favorite_count = favorite_count,
              politician = figure
            ) %>%
            select(status_id, created_at, text, user_id, location, retweet_count, favorite_count, politician)
          
          all_tweets[[length(all_tweets) + 1]] <- tweet_data
        }
      }
      
      # Sleep to respect rate limits
      Sys.sleep(2)
      
    }, error = function(e) {
      log_error(sprintf("Error collecting tweets for %s: %s", figure, e$message))
    })
  }
  
  # Save collected tweets
  if (length(all_tweets) > 0) {
    df <- bind_rows(all_tweets)
    timestamp <- format(now(), "%Y%m%d_%H%M%S")
    output_path <- sprintf("data/twitter/tweets_%s.csv", timestamp)
    write_csv(df, output_path)
    log_info(sprintf("Saved %d Twitter posts to %s", nrow(df), output_path))
  } else {
    log_warning("No Twitter data collected")
  }
}

# ----------------------
# Reddit Data Collection
# ----------------------
collect_reddit_data <- function() {
  log_info("Starting Reddit data collection")
  
  # Subreddits and political figures
  subreddits <- c("politics", "news", "PoliticalDiscussion", "NeutralPolitics")
  political_figures_extended <- c(
    "Donald Trump", "President Trump", "POTUS",
    "Chuck Schumer", "Charles Schumer",
    "Mitch McConnell",
    "Mike Johnson", "Speaker Johnson",
    "Hakeem Jeffries"
  )
  
  all_posts <- list()
  
  # Define date range (Q1 2025, extended to today)
  start_epoch <- as.numeric(as.POSIXct("2025-01-01", tz = "UTC"))
  end_epoch <- as.numeric(as.POSIXct("2025-04-20", tz = "UTC"))
  
  for (subreddit_name in subreddits) {
    tryCatch({
      log_info(sprintf("Collecting posts from r/%s", subreddit_name))
      
      # Collect Reddit posts
      posts <- reddit_urls(
        subreddit = subreddit_name,
        sort_by = "new",
        page_limit = 10  # Approx 1000 posts (100 per page)
      )
      
      if (nrow(posts) > 0) {
        # Convert created_utc to numeric for comparison
        posts <- posts %>%
          mutate(created_utc = as.numeric(created_utc))
        
        for (i in 1:nrow(posts)) {
          post <- posts[i, ]
          post_content <- tolower(paste(post$title, post$comment, sep = " "))
          
          # Check if post mentions any political figure
          matching_figures <- political_figures_extended[sapply(
            political_figures_extended,
            function(figure) grepl(tolower(figure), post_content)
          )]
          
          if (length(matching_figures) > 0 &&
              post$created_utc >= start_epoch &&
              post$created_utc <= end_epoch) {
            figure <- matching_figures[1]  # Use the first matching figure
            post_data <- data.frame(
              post_id = post$post_id,
              timestamp = as.POSIXct(post$created_utc, origin = "1970-01-01", tz = "UTC"),
              subreddit = post$subreddit,
              title = post$title,
              text = post$comment,
              score = post$score,
              comment_count = post$num_comments,
              politician = figure,
              stringsAsFactors = FALSE
            )
            all_posts[[length(all_posts) + 1]] <- post_data
            log_info(sprintf("Collected post ID %s for '%s'", post$post_id, figure))
            
            # Fetch comments
            tryCatch({
              comments <- reddit_content(post$url, wait_time = 1)
              comment_count <- 0
              
              if (length(comments) > 0 && "comment" %in% names(comments)) {
                comments_df <- comments %>%
                  as.data.frame() %>%
                  mutate(
                    comment_timestamp = as.POSIXct(
                      as.numeric(comment_date),
                      origin = "1970-01-01",
                      tz = "UTC"
                    )
                  ) %>%
                  filter(
                    comment_timestamp >= as.POSIXct(start_epoch, origin = "1970-01-01", tz = "UTC") &
                      comment_timestamp <= as.POSIXct(end_epoch, origin = "1970-01-01", tz = "UTC")
                  )
                
                for (j in 1:min(1000, nrow(comments_df))) {
                  comment <- comments_df[j, ]
                  comment_data <- data.frame(
                    post_id = post$post_id,
                    comment_id = comment$comment_id,
                    timestamp = comment$comment_timestamp,
                    subreddit = post$subreddit,
                    text = comment$comment,
                    score = as.integer(comment$comment_score),
                    politician = figure,
                    stringsAsFactors = FALSE
                  )
                  all_posts[[length(all_posts) + 1]] <- comment_data
                  comment_count <- comment_count + 1
                }
              }
              log_info(sprintf("Collected %d comments for post ID %s", comment_count, post$post_id))
              Sys.sleep(1)  # Delay after comments
              
            }, error = function(e) {
              log_error(sprintf("Error collecting comments for post ID %s: %s", post$post_id, e$message))
            })
          }
        }
      }
      
      Sys.sleep(2)  # Delay between subreddits
      
    }, error = function(e) {
      log_error(sprintf("Error collecting Reddit data from r/%s: %s", subreddit_name, e$message))
    })
  }
  
  # Save collected Reddit data
  if (length(all_posts) > 0) {
    df <- bind_rows(all_posts)
    timestamp <- format(now(), "%Y%m%d_%H%M%S")
    output_path <- sprintf("data/reddit/reddit_posts_%s.csv", timestamp)
    write_csv(df, output_path)
    log_info(sprintf("Saved %d Reddit posts/comments to %s", nrow(df), output_path))
  } else {
    log_warning("No Reddit data collected")
  }
}

# ----------------------
# News Commentary Collection
# ----------------------
collect_news_comments <- function() {
  log_info("Starting news commentary collection")
  
  # News sites to scrape comments from
  news_sites <- list(
    list(
      name = "CNN",
      url = "https://www.cnn.com/politics",
      comment_selector = ".comment-item"  # Placeholder
    ),
    list(
      name = "Fox News",
      url = "https://www.foxnews.com/politics",
      comment_selector = ".comment"  # Placeholder
    ),
    list(
      name = "New York Times",
      url = "https://www.nytimes.com/section/politics",
      comment_selector = ".css-comment-container"  # Placeholder
    ),
    list(
      name = "Wall Street Journal",
      url = "https://www.wsj.com/news/politics",
      comment_selector = ".comment-content"  # Placeholder
    )
  )
  
  all_comments <- list()
  
  # For each news site
  for (site in news_sites) {
    tryCatch({
      log_info(sprintf("Collecting from %s", site$name))
      
      # Get main page
      response <- GET(site$url, add_headers("User-Agent" = "Mozilla/5.0"))
      page <- content(response, as = "text")
      soup <- read_html(page)
      
      # Find article links
      article_links <- soup %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        unique()
      
      # Filter links mentioning political figures
      article_links <- article_links[sapply(article_links, function(link) {
        link_text <- tryCatch({
          page_link <- GET(link, add_headers("User-Agent" = "Mozilla/5.0"))
          page_content <- content(page_link, as = "text")
          page_soup <- read_html(page_content)
          html_text(page_soup)
        }, error = function(e) "")
        
        any(sapply(political_figures, function(figure) grepl(tolower(figure), tolower(link_text))))
      })]
      
      # Process a limited number of articles
      for (link in head(article_links, 5)) {  # Limit to 5 articles per site
        tryCatch({
          # Get article (simplified, no equivalent to newspaper3k in R)
          article_response <- GET(link, add_headers("User-Agent" = "Mozilla/5.0"))
          article_page <- content(article_response, as = "text")
          article_soup <- read_html(article_page)
          article_title <- article_soup %>%
            html_node("title") %>%
            html_text()
          
          # Simulate comment collection (placeholder)
          for (figure in political_figures) {
            article_text <- html_text(article_soup)
            if (grepl(tolower(figure), tolower(article_text))) {
              comment_data <- data.frame(
                comment_id = paste0("simulated_", digest::digest(link), "_", digest::digest(figure)),
                source = site$name,
                article_title = article_title,
                article_url = link,
                timestamp = now(),
                text = sprintf("This is a placeholder for comment data that would be extracted from %s", site$name),
                politician = figure,
                stringsAsFactors = FALSE
              )
              all_comments[[length(all_comments) + 1]] <- comment_data
            }
          }
          
          Sys.sleep(3)  # Delay to be respectful
          
        }, error = function(e) {
          log_error(sprintf("Error processing article %s: %s", link, e$message))
        })
      }
      
    }, error = function(e) {
      log_error(sprintf("Error collecting from %s: %s", site$name, e$message))
    })
  }
  
  # Save collected comments
  if (length(all_comments) > 0) {
    df <- bind_rows(all_comments)
    timestamp <- format(now(), "%Y%m%d_%H%M%S")
    output_path <- sprintf("data/news_comments/news_comments_%s.csv", timestamp)
    write_csv(df, output_path)
    log_info(sprintf("Saved %d news comments to %s", nrow(df), output_path))
  } else {
    log_warning("No news comments collected")
  }
}

# ----------------------
# Main execution
# ----------------------
main <- function() {
  log_info("Starting data collection process")
  
  # Collect data from each source
  collect_twitter_data()
  collect_reddit_data()
  collect_news_comments()
  
  log_info("Data collection complete")
}

# Run the script
main()