#Packages used
library(tidyverse)
library(scales)
library(scriptuRs)
library(lubridate)
library(tidytext)
library(ggthemes)

# Conference text data
conf <- read.csv('datasets/conference.csv', stringsAsFactors = FALSE)
tidy_conf <- read.csv('datasets/tidy_conf.csv', stringsAsFactors = FALSE)
conf_topic <- read.csv('datasets/talks_by_topic_dec_2020.csv', stringsAsFactors = FALSE)
# Convert text to tidy table format, removing stop words
#tidy_conf <- conf %>%
#  filter(!is.na(text)) %>%
#  arrange(desc(year), desc(month)) %>%
#  unnest_tokens(word, text) %>%
#  anti_join(stop_words) %>%
#  filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

#write_csv(tidy_conf, 'datasets/tidy_conf.csv')
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

unique_speakers <- sort(unique(tidy_conf$speaker))

# Function that McKay wrote: Finds the frequency of a word
word_conf <- function(word, whole = TRUE, data) {
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
conf_wordcloud <- function(year, session){
  library(wordcloud)
  pal <- brewer.pal(8, "Paired")
  dark_pal <- pal %>% gt::adjust_luminance(-1.0)
  tidy_conf %>% 
    filter(year == year, month == session) %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, random.order = TRUE, max.words = 100, colors=dark_pal, rot.per = .2, scale = c(3.5, .25)))
}
