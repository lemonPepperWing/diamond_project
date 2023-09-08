### SENTIMENT ANALYSIS PLOT 

# load libraries
library(tidyverse)
library(tidytext)

# create a tibble to work with the dataset
df <- tibble(line = seq_along(sentences), text = sentences)

# unnest each word in each sentence as its own row
df_unnest <- df %>% 
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word))

df_unnest

# filter out stop words
df_filtered <- df_unnest %>%
  anti_join(stop_words)

# get sentiments for each word
sentiment <- get_sentiments("bing") 

sentiment %>% head()

# inner join the bing sentiment to the word tibble
df_sentiment <- df_filtered %>%
  inner_join(sentiment)

df_sentiment

# count the top ten Harvard words
df_sentiment %>%
  group_by(word) %>% 
  summarise(n = n(), 
            sentiment = first(sentiment)) %>%
  arrange(desc(n)) %>% 
  head(n=10)

# plot a bar chart of the most common Harvard words
df_sentiment %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "midnightblue") +
  labs(y = NULL)


# calculate words sentiment by score:
harvard_score_sentiments <- df_sentiment %>%
  group_by(word) %>%
  summarize(
    n = n(),
    negative = sum(sentiment == "negative"),
    positive = sum(sentiment == "positive")
  ) %>%
  mutate(norm_neg = negative / n,
         norm_pos = positive / n,
         sentiment_score = norm_pos - norm_neg)

# plot a bar chart of the top Harvard sentiment words (+/-)  
harvard_score_sentiments %>% 
  mutate(mood = ifelse(sentiment_score < 0, 'More Negative', 
                       'More Positive')) %>%
  slice_max(n, n = 15) %>% 
  ggplot() +
  geom_col(aes(
      x = reorder(word, n),
      y = n,
      fill = mood)
      ) + 
  facet_wrap(~mood, scales = "free") +
  coord_flip() +
  labs(y=" Top Harvard sentiment words", x = NULL) +
  theme_bw() + 
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = 'bold'),
    strip.text.x = element_text(size = 12, face = 'bold')
  )
