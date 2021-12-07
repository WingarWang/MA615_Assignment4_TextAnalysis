############################## prepare
############################## library
#install.packages("devtools")
library(devtools)
#devtools::install_github("Truenumbers/tnum/tnum")
library(tnum)

############################## input the book
library(gutenbergr)
alice <- gutenberg_download(11)

############################## clear up
library(dplyr)
library(stringr)
alice1 <- alice %>%
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
alice2 <- alice %>%
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

############################## task 1
############################## lexicons
library(tidytext)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

##############################
##############################
##############################





