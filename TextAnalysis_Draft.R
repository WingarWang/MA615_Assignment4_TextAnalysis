############################## prepare
############################## input the book
library(gutenbergr)
alice0 <- gutenberg_download(gutenberg_id = 11)

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
          unnest_tokens(word, text) 

#change ’ to '
alice2$word <- gsub("’","'",alice2$word)

alice2 <- alice2 %>%
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
library(tidyr)
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
  with(wordcloud(word, n, max.words = 55))

library(reshape2)
alice2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 50)

############################## task 2: sentence-level analysis
############################## library
#install.packages("devtools")
library(devtools)
#devtools::install_github("Truenumbers/tnum/tnum")
library(tnum)

library(knitr)

tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test2")
tnum.getSpace()
tnum.setSpace("test3")
tnum.getSpace()

############################## TNs
alice3 <- gutenberg_download(gutenberg_id = 11)
alice3$text <- gsub("’","'",alice3$text)
alice3$text <- gsub('“','"',alice3$text)
alice3$text <- gsub('”','"',alice3$text)
alice3$text <- gsub('—','',alice3$text)
alice3$text <- gsub('_','',alice3$text)
alice3$text <- gsub("‘","'",alice3$text)

writeLines(alice3$text, "alice_text.txt")
alice4 <- readLines("alice_text.txt")
show(alice4)

#WRONG: appear chinese and messy codes:
#writeLines(alice0$text, "alice_text.txt")
#alice5 <- readLines("alice_text.txt")
#show(alice5)
#simple version code above:
#alice5 <- alice0$text
 
#WRONG: I copy the words on the website, but there are still messy codes:
#alice6 <- readLines("alice_text_2.txt")
#show(alice6)

source("Book2TN-v6A-1.R")

#test:
#test <- readLines("alice_test.txt")
#show(test)
#tnBooksFromLines(test, "just_a_test/test") 

tnBooksFromLines(alice4, "carroll/alice")
#past name:
#carroll/alice
#LewisCarroll/AliceWonderland
tnum.getDBPathList(taxonomy = "subject", levels=2)
tnum.getDBPathList(taxonomy = "subject", levels=1)

#tnum.query("LewisCarrol/AliceWonderland# has *")

############################## explore
##  The ordinal numbers for the entire book 
##  show the sequence of objects in order of their appearance.
w10 <- tnum.query("carroll/alice# has ordinal", max=1800)
wdf10 <- tnum.objectsToDf((w10))

## Examing the first 50 TNs  makes it easy to see the Table of Contents
## and to see that object 22 is the heading at the start of Chapter 1
## This shows the Table of Contents
w11 <- tnum.query("carroll/alice# has text", start = 3 ,max=18)
wdf11 <- tnum.objectsToDf((w11))

table_of_contents <- wdf11 %>% select(string.value) 

w12 <- tnum.query("carroll/alice# has text", start = 3 ,max=18)
wdf12 <- tnum.objectsToDf((w12))

## Look at just the headings shows the structure of the book
w13 <- tnum.query("carroll/alice/heading# has text", max=40) #############
wdf13 <- tnum.objectsToDf(w13)

## It may look like the table of contents is repeated twice,
## but examing the ordinals produces chapter list that includes the 
## ordinal location for the heading of each chapter
w14 <- tnum.query("carroll/alice/heading# has ordinal", max=40) #############
wdf14 <- tnum.objectsToDf(w14)

chapter_locations <- left_join(select(wdf13, subject, string.value), 
                               select(wdf14, subject, numeric.value)) %>% 
  slice(22:38)
## add column for chapter number
chapter_locations %<>% mutate(chapter=1:17)

w15 <- tnum.query("carroll/alice/section:0022# has ordinal")
wdf15 <- tnum.objectsToDf(w15)

##############################
a <- chapter_locations %>% filter(chapter==2) %>% 
  select(numeric.value) %>% 
  unlist()
a <- str_pad(as.character(a),4,side="left",pad="0")

b <- paste0("carroll/alice/section:",a,"#", " has ordinal")
b

w16 <- tnum.query("carroll/alice/section:0022# has ordinal")

##############################
## chapter 1 para 1, word counts for the 3 sentences in para 1
q20 <- tnum.query("carroll/alice# has *", max=3)
df20 <- tnum.objectsToDf(q20)

#  chapter locations  ordinal numbers
ord_ch1 <- unlist(tnum.query("carroll/alice/heading:0022# has ordinal"))
ord_ch2 <- unlist(tnum.query("carroll/alice/heading:0023# has ordinal"))


ch1_txt <- tnum.query("carroll/alice/section:0022/paragraph:0002/# has text", max=30)

ch1_txt_df <- tnum.objectsToDf(ch1_txt)
ch1_txt_df$string.value



ch2_txt <- tnum.query("carroll/alice/section:0022/paragraph:0002/sentence:# has *", max=30)
ch2_txt_df <- tnum.objectsToDf(ch2_txt)

ch2_txt_df$string.value

length(ch2_txt_df$string.value)


q21 <- tnum.query("carroll/alice/section:0022/paragraph:0001/# has *", max = 30)
df21 <- tnum.objectsToDf(q21)

w20 <- tnum.query()

############################## task 3: sentimentr
##############################
library(sentimentr)

dq_text <- filter(df1, property == "text")
line_ex <- dq_text$string.value[2178]
sentiment(line_ex)
sents <- sentiment(get_sentences(dq_text))

dq_text %<>% separate(col = subject, into = c("author", "book", "chapter", "paragraph","sentence"), sep = "/", fill = "right")

sents_chapter <- sentiment_by(get_sentences(dq_text), by = 'chapter')
# sents_chapter <- filter(sents_chapter, word_count > 100) %>%
#   mutate(sents_chapter, chapter = substr(chapter, 9,12)) %>%
#   mutate(chapter = as.numeric(chapter))

ggplot(sents_chapter, aes(chapter, ave_sentiment)) +
  geom_col(show.legend = FALSE)














