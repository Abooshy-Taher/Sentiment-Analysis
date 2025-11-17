###############################################
# SECTION 1 — Packages
###############################################
pkgs <- c("tidyverse", "lubridate", "tidytext", "jsonlite")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

###############################################
# SECTION 2 — Load Dataset
###############################################
if (exists("Amazon_reviews_2023")) {
  df <- Amazon_reviews_2023
} else {
  df <- readr::read_csv("~/Desktop/Amazon_reviews_2023.csv")
}

glimpse(df)
cols <- names(df)

###############################################
# SECTION 3 — Auto-Detect Columns
###############################################
pick <- function(candidates, cols) {
  hit <- intersect(candidates, cols)
  if (length(hit) == 0) NA_character_ else hit[1]
}

time_col   <- pick(c("reviewTime","review_time","timestamp","time","review_date"), cols)
rating_col <- pick(c("overall","star_rating","rating"), cols)
text_col   <- pick(c("reviewText","review_text","review_body","text","body"), cols)

cat("TIME:", time_col, "\nRATING:", rating_col, "\nTEXT:", text_col, "\n")

###############################################
# SECTION 4 — Timestamp Parsing
###############################################
if (!is.na(time_col)) {
  df <- df %>%
    mutate(
      review_time_raw   = .data[[time_col]],
      review_time_fixed = lubridate::parse_date_time(review_time_raw,
                                                     orders = c("ymd HMS", "ymd HM", "ymd H", 
                                                                "ymd", "mdy HMS", "mdy HM", 
                                                                "mdy", "Ymd HMS", "Ymd HM",
                                                                "Ymd", "dby HMS", "dby", "dbY")),
      review_date = as.Date(review_time_fixed),
      review_hour = lubridate::hour(review_time_fixed)
    )
}

###############################################
# SECTION 5 — Feasibility Checks
###############################################
if (!is.na(text_col))  cat("Text rows:", sum(!is.na(df[[text_col]])), "\n")
if (!is.na(rating_col)) print(table(df[[rating_col]], useNA="ifany"))
if ("review_hour" %in% names(df)) print(df %>% count(review_hour))

###############################################
# SECTION 6 — Mini Sentiment Experiment
###############################################
if (!is.na(text_col) && !is.na(rating_col)) {
  
  bing_lex <- tidytext::get_sentiments("bing")
  
  sentiment_df <- df %>%
    filter(!is.na(.data[[text_col]]),
           !is.na(.data[[rating_col]])) %>%
    select(text = all_of(text_col),
           rating = all_of(rating_col)) %>%
    unnest_tokens(word, text) %>%
    inner_join(bing_lex, by = "word") %>%
    mutate(sentiment_score = if_else(sentiment == "positive", 1L, -1L)) %>%
    group_by(rating) %>%
    summarise(avg_sentiment = mean(sentiment_score),
              n_words = n(),
              .groups = "drop")
  
  print(sentiment_df)
  
  ###############################################
  # SECTION 7 — Visualization
  ###############################################
  ggplot(sentiment_df, aes(x = factor(rating), y = avg_sentiment)) +
    geom_col() +
    labs(title = "Average Sentiment Score by Rating",
         x = "Star Rating",
         y = "Sentiment Score")
}
###############################################
# SENTIMENT vs HOUR OF DAY
###############################################

hour_sentiment <- df %>%
  select(text, review_hour) %>%
  unnest_tokens(word, text) %>%
  inner_join(tidytext::get_sentiments("bing"), by="word") %>%
  mutate(sent_score = if_else(sentiment == "positive", 1, -1)) %>%
  group_by(review_hour) %>%
  summarise(avg_sentiment = mean(sent_score),
            n_words = n(),
            .groups = "drop")

print(hour_sentiment)

# Visualization
ggplot(hour_sentiment, aes(x = review_hour, y = avg_sentiment)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Sentiment Score by Hour of Day",
       x = "Hour of Day (0–23)",
       y = "Average Sentiment Score")
