############################## prepare
############################## input the book
library(gutenbergr)
alice0 <- gutenberg_download(11)

############################## clear up
library(dplyr)
library(stringr)
alice1 <- alice0 %>%
          select(-gutenberg_id) %>%
          mutate(linenumber = row_number(),
          chapter = cumsum(str_detect(text, 
                                      regex("^chapter [\\divxlc]",
                                      ignore_case = TRUE)))) %>%
          filter(linenumber >= 30) %>%
          ungroup()

############################## tidy
library(tidytext)
data(stop_words)
alice2 <- alice1 %>%
          unnest_tokens(word, text) %>%
          anti_join(stop_words)

############################## check
alice2 %>%
       count(word, sort = TRUE)

library(ggplot2)
alice2 %>%
       count(word, sort = TRUE) %>%
       filter(n > 50) %>%
       mutate(word = reorder(word, n)) %>%
       ggplot(aes(n, word)) +
       geom_col() +
       labs(y = NULL)

############################## task 1: bag of word analysis
############################## lexicons
library(tidytext)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

############################## nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

alice2 %>%
       inner_join(nrc_joy) %>%
      count(word, sort = TRUE)

############################## bing
alice_sentiment <- alice2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(alice_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

############################## comparing the three sentiment dictionaries
afinn <- alice2 %>% 
         inner_join(get_sentiments("afinn")) %>% 
         group_by(index = linenumber %/% 80) %>% 
         summarise(sentiment = sum(value)) %>% 
         mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
                alice2 %>% 
                       inner_join(get_sentiments("bing")) %>%
                        mutate(method = "Bing et al."),
                alice2 %>% 
                       inner_join(get_sentiments("nrc") %>% 
                       filter(sentiment %in% c("positive", "negative"))) %>%
                       mutate(method = "NRC")) %>%
                count(method, index = linenumber %/% 80, sentiment) %>%
                pivot_wider(names_from = sentiment,
                            values_from = n,
                            values_fill = 0) %>% 
                mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
                               ggplot(aes(index, sentiment, fill = method)) +
                               geom_col(show.legend = FALSE) +
                               facet_wrap(~method, ncol = 1, scales = "free_y")

############################## most common positive and negative words
bing_word_counts <- alice2 %>%
                           inner_join(get_sentiments("bing")) %>%
                           count(word, sentiment, sort = TRUE) %>%
                           ungroup()
bing_word_counts

bing_word_counts %>%
                 group_by(sentiment) %>%
                 slice_max(n, n = 10) %>% 
                 ungroup() %>%
                 mutate(word = reorder(word, n)) %>%
                 ggplot(aes(n, word, fill = sentiment)) +
                 geom_col(show.legend = FALSE) +
                 facet_wrap(~sentiment, scales = "free_y") +
                 labs(x = "Contribution to sentiment", y = NULL)

############################## wordclouds
library(wordcloud)
alice2 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 70))

library(reshape2)
alice2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 60)

############################## task 2: sentence-level analysis
############################## library
#install.packages("devtools")
library(devtools)
#devtools::install_github("Truenumbers/tnum/tnum")
library(tnum)

source("Book2TN-v6A-1.R")
tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test2")


##############################
##############################
##############################














