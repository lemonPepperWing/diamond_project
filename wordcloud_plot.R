###

# load libraries
library(tidyverse)
library(tidytext)
library(reshape2)
library(wordcloud)

# create a tibble to work with the dataset
df <- tibble(line = seq_along(sentences), text = sentences)

# unnest each word in each sentence as its own row
df_unnest <- df %>% 
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word))

df_unnest

# plot the common words as a wordcloud
df_unnest %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# plot the wordcloud to tag positive and negative words 
df_unnest %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "yellow"),
                   max.words = 100)

