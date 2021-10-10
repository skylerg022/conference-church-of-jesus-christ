#Packages used
library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(ggthemes)
library(stopwords)

# Conference text data

conf <- readRDS("rdsfiles/conf.rds")
tidy_conf <- readRDS("rdsfiles/tidy_conf.rds")
conf_topic <- readRDS("rdsfiles/conf_topic.rds")
#russian <- readRDS("rdsfiles/conf_ru.rds")
#tidy_conf_ru <- readRDS("rdsfiles/tidy_conf.rds")
# Convert text to tidy table format, removing stop words
#tidy_conf <- conf %>%
#  filter(!is.na(text)) %>%
#  arrange(desc(year), desc(month)) %>%
#  unnest_tokens(word, text) %>%
#  anti_join(stop_words) %>%
#  filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

#write_csv(tidy_conf, 'datasets/tidy_conf.csv')

# russian_stop <- stopwords(language = "ru")
# stop_words_ru <- data.frame(word = russian_stop, lexicon = "Snowball")

# tidy_conf_ru <- russian %>%
#   filter(!is.na(text)) %>%
#   arrange(desc(year), desc(month)) %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words_ru) %>%
#   filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))
# 
# write.csv(tidy_conf_ru, 'datasets/tidy_conf_ru.csv')

topic_speaker <- conf_topic %>%
  mutate(topic = str_trim(topic)) %>%
  group_by(speaker) %>%
  count(topic, sort = TRUE) %>%
  mutate(topic = str_to_title(topic)) %>%
  ungroup()

tidy_conf_speaker <- tidy_conf %>%
  group_by(speaker) %>%
  count(word, sort = TRUE) %>%
  mutate(word = str_to_title(word)) %>%
  ungroup()

# tidy_conf_speaker_ru <- tidy_conf_ru %>%
#   group_by(speaker) %>%
#   count(word, sort = TRUE) %>%
#   mutate(word = str_to_title(word)) %>%
#   ungroup()

unique_speakers <- sort(unique(tidy_conf$speaker))
#unique_speakers_ru <- sort(unique(tidy_conf_ru$speaker))

# Function that McKay wrote: Finds the frequency of a word
word_conf_en <- function(word, whole = TRUE, data) {
  x <- data
  x$yearmonth <- round(x$year + (x$month/12), 2)
  wor <- tolower(word)
  if (whole == FALSE) x$word_count <- str_count(tolower(x$text), wor)
  if (whole == TRUE) x$word_count <- str_count(tolower(x$text), paste0("[\\s[:punct:]]",wor,"[\\s[:punct:]]"))
  
  x <- x %>% group_by(yearmonth) %>% summarize("word_count" = sum(word_count))
  return(ggplot(x, aes(x = yearmonth, y = word_count)) + geom_line() + 
           geom_point(aes(x = pull(x[which.min(word_count),"yearmonth"]),y=pull(x[which.min(word_count),"word_count"])), size =6, color = "blue") + 
           geom_point(aes(x = pull(x[which.max(word_count),"yearmonth"]), y = pull(x[which.max(word_count),"word_count"])), size = 6, color = "red") +
           labs(x = "Year", y = "Word Count", title = paste0("Word Count by Conference: ",str_to_title(wor))) +
           theme_classic() + theme(axis.title = element_text(size = 14)))
}

# word_conf_ru <- function(word, whole = TRUE, data) {
#   x <- data
#   x$yearmonth <- round(x$year + (x$month/12), 2)
#   wor <- tolower(word)
#   if (whole == FALSE) x$word_count <- str_count(tolower(x$text), wor)
#   if (whole == TRUE) x$word_count <- str_count(tolower(x$text), paste0("[\\s[:punct:]]",wor,"[\\s[:punct:]]"))
#   
#   x <- x %>% group_by(yearmonth) %>% summarize("word_count" = sum(word_count))
#   return(ggplot(x, aes(x = yearmonth, y = word_count)) + geom_line() + 
#            geom_point(aes(x = pull(x[which.min(word_count),"yearmonth"]),y=pull(x[which.min(word_count),"word_count"])), size =6, color = "blue") + 
#            geom_point(aes(x = pull(x[which.max(word_count),"yearmonth"]), y = pull(x[which.max(word_count),"word_count"])), size = 6, color = "red") +
#            labs(x = "Год", y = "Количество слов", title = paste0("Количество слов по конференции: ",str_to_title(wor))) +
#            theme_classic() + theme(axis.title = element_text(size = 14)))
# }

topic_conf <- function(topic_choice, data) {
  x <- data %>% mutate(yearmonth = round(month + (year/12), digits = 2),
                       topic = str_to_title(str_trim(topic))) %>% 
    group_by(yearmonth, topic) %>% 
    summarize(topic_count = n()) %>% 
    filter(topic == topic_choice)
  return(ggplot(x, aes(x = yearmonth, y = topic_count)) + geom_line() + 
           geom_point(aes(x = pull(x[which.min(topic_count),"yearmonth"]),y=pull(x[which.min(topic_count),"topic_count"])), size =6, color = "blue") + 
           geom_point(aes(x = pull(x[which.max(topic_count),"yearmonth"]), y = pull(x[which.max(topic_count),"topic_count"])), size = 6, color = "red") +
           labs(x = "Year", y = "Topic Count", title = paste0("Topic Count by Conference: ",str_to_title(topic_choice))) +
           theme_classic() + theme(axis.title = element_text(size = 14)))
}

unique_topics <- sort(unique(topic_speaker$topic))
# Function to create a word cloud
conf_wordcloud <- function(year, session, data){
  #pal <- brewer.pal(8, "Paired")
  #dark_pal <- pal %>% gt::adjust_luminance(-1.0)
  data %>% 
    filter(year == year, month == session) %>%
    count(word, sort = TRUE) %>% head(150) %>%
    wordcloud2(color = "random-dark") + WCtheme(1)
}
