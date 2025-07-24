### This script contains text analysis code.
# 1. Set up and loading
# 2. Basic summary
# 3. Sentiment analysis
# 3.1. Bing
# 3.2. Afinn
# 4. Word cloud

# All `ggsave` commands are commented to prevent the code from running when 
# calling `source(script)`.

### 1. Set up and loading
library(tidyverse)
library(tidytext)
library(textdata)
library(ggwordcloud)
library(reshape2)

# to reorder period
period_lvl <- c("[1950,2013]", "(2013,2020]", "(2020,2025]")

reviews_df <- read_csv("data/reviews.csv")
movies_df <- read_csv("data/movies.csv")
reviews_joined <- reviews_df %>%
  left_join(movies_df %>% select(id, year, period), by = "id")
reviews_joined$period <- factor(reviews_joined$period, levels = period_lvl)

glimpse(reviews_joined)

### 2. Basic summary

# tokenize
reviews_words <- reviews_joined %>%
  filter(!is.na(review)) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)

# Inspect result
# reviews_words
bad_words <- c("film", "movie", "plot", "story", "characters")

# Plot: Most frequents words
p_most_words <- reviews_words %>%
  group_by(period) %>%
  filter(!word %in% bad_words) %>%
  count(word, sort = T) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder_within(word, n, period)) %>%
  ggplot(aes(word, n, fill = period)) +
  geom_col(width = 0.7) +
  scale_x_reordered() +
  facet_wrap(~period, scales = "free_y") +
  labs(
    title = "Most Frequent Words by Period",
    x = NULL,
    y = "Count"
  ) +
  guides(fill = "none") +
  coord_flip()
# ggsave(filename = "img/most_frequent_period.png", plot = p_most_words, width = 8, height = 4.5)

### 3. Sentiment analysis

# 3.1. Join Bing lexicon
reviews_bing <- reviews_words %>%
  inner_join(get_sentiments("bing")) %>%
  filter(!(word %in% bad_words))

# Plot: Top 10 Positive & Negative Words
p_sentiment <- reviews_bing %>% 
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_manual(values = c(
    "positive" = as.character(solarized_pal("cyan")(1)), 
    "negative" = as.character(solarized_pal("magenta")(1))
    )) +
  geom_col(show.legend = FALSE, width = 0.7) +
  scale_x_reordered() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    title = "Most Frequent Sentiment Words in Reviews",
    x = NULL, y = "Count"
  ) +
  coord_flip()
ggsave(filename = "img/sentiment.png", plot = p_sentiment, width = 6, height = 4)

# Plot: period + sentiment
sentiment_period_data <- reviews_bing %>%
  count(period, sentiment, word, sort = TRUE) %>%
  group_by(period, sentiment)

plot_single_sentiment_period <- function(sentiment, color) {
  sentiment_period_data %>%
    filter(sentiment == {{ sentiment }}) %>%
    slice_max(order_by = n, n = 10) %>%
    mutate(word = reorder_within(word, n, period)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill = color, width = 0.7, show.legend = FALSE) +
    scale_x_reordered() +
    facet_wrap(~period, scales = "free_y") +
    labs(
      title = paste0("Frequent ", as.character({{ sentiment }}), " words by period"),
      x = NULL, y = "Word Count"
    ) +
    coord_flip()
}

p_period_positive <- plot_single_sentiment_period("positive", as.character(solarized_pal("cyan")(1)))
# ggsave(filename = "img/by_period_positive.png", plot = p_period_positive, width = 8)
p_period_negative <- plot_single_sentiment_period("negative", as.character(solarized_pal("magenta")(1)))
# ggsave(filename = "img/by_period_negative.png", plot = p_period_negative, width = 8)

# combine period sentiment plots
p_period_negative <- p_period_negative +  
  labs(title = NULL)
p_period_pos_neg <- p_period_positive + p_period_negative + plot_layout(
  axis_titles = "collect", guides = "collect", axes = "collect") 
# ggsave(filename = "img/period_sentiment.png", plot = p_period_pos_neg, width = 16, height = 9) 

# 3.2. Join AFINN lexicon
reviews_afinn <- reviews_words %>%
  inner_join(get_sentiments("afinn"))

# Aggregate sentiment score by review
afinn_sum <- reviews_afinn %>%
  group_by(id) %>%
  summarize(sentiment_score = mean(value)) %>%
  left_join(movies_df) %>%
  distinct(id, .keep_all = T) %>%
  mutate(title_wrapped = str_wrap(title, width = 15))

afinn_sum$period <- factor(afinn_sum$period, levels = period_lvl)


# Plot distribution of sentiment scores 
# versus user ratings
p_sentiment_ratings <- afinn_sum %>%
  filter(!is.na(vote_average)) %>%
  mutate(vote_average_bin = floor(vote_average)) %>%  # Bin by 1
  group_by(vote_average_bin, period) %>%
  summarise(mean_sentiment = mean(sentiment_score, na.rm = TRUE),
            total_vote = sum(vote_count),
            .groups = "drop") %>%
  mutate(label_y = 0.5 * mean_sentiment) %>% 
  ggplot(aes(x = factor(vote_average_bin), y = mean_sentiment, fill = period)) +
  geom_col(aes(alpha = total_vote)) +
  scale_alpha_continuous(range = c(0.15, 1)) +
  geom_text(aes(y = label_y, label = comma(total_vote)), size = 2.5, vjust = -0.2) +
  labs(
    title = "Mean Sentiment Score (AFINN) by User Rating",
    subtitle = "Ratings are binned by 1, Opacity denotes number of vote",
    x = "Ratings", y = "Mean Sentiment Score"
  ) +
  facet_wrap(~period, scales = "fixed") +
  guides(size = "none", alpha = "none", fill = "none") 
# ggsave(filename = "img/sentiment_ratings.png", plot = p_sentiment_ratings, width = 8, height = 4.5)

# versus revenue
p_sentiment_revenue <- afinn_sum %>% 
  filter(!is.na(revenue)) %>% 
  ggplot(aes(x = sentiment_score, y = revenue / (10 ** 6))) +
  geom_point(aes(color = period)) +
  ggrepel::geom_text_repel(
    data = afinn_sum %>% 
      filter(!is.na(period) & sentiment_score < -50 | revenue > 10**8),
    aes(label = title_wrapped),
    force = 2,
    size = 3
  ) +
  geom_vline(mapping = aes(color = "gray"), xintercept = 0) +
  labs(
    x = "Sentiment Score",
    y = "Revenues (million USD)"
  ) +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~period, scales = "fixed") +
  guides(color = "none") +
  coord_flip()
# ggsave(filename = "img/sentiment_revenue.png", plot = p_sentiment_revenue, width = 8, height = 4.5)

# combine sentiment ~ revenue + ratings
p_sentiment_rating_rev <- p_sentiment_ratings + p_sentiment_revenue + plot_layout(
  guides = "collect", axis_titles = "collect", axes = "collect"
) &
  theme(legend.position = 'bottom')
# ggsave(filename = "img/sentiment_rating_revenue.png", plot = p_sentiment_rating_rev, width = 10, height = 5.625)


### 4. Word cloud by frequency
# just to have pretty img
set.seed(123)
word_cloud <- reviews_words %>%
  filter(!word %in% bad_words) %>% 
  count(word, sort = TRUE) %>%
  slice_head(n = 50) %>%
  mutate(
    angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(0.7, 0.3)),
    color = sample(c("gray40", "steelblue", "firebrick"), n(), replace = TRUE)
  ) %>%
  ggplot(aes(label = word, size = n, angle = angle, color = color)) +
  geom_text_wordcloud_area(
    rm_outside = TRUE,
    family = "serif",
    eccentricity = 0.7
  ) +
  scale_size_area(max_size = 50) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(caption = "Word cloud of environmental movies") +
  scale_color_identity() # use the colors as given directly 
# ggsave(filename = "img/word_cloud.png", plot = word_cloud, width = 8, height = 4)
