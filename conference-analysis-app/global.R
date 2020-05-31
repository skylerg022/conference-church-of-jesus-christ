#Packages used
library(tidyverse)
library(scales)
library(scriptuRs)
library(lubridate)
library(tidytext)
library(ggthemes)

# Conference text data
conf <- read.csv('datasets/conference.csv', stringsAsFactors = FALSE)

# Convert text to tidy table format, removing stop words
tidy_conf <- conf %>%
  filter(!is.na(text)) %>%
  arrange(desc(year), desc(month)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

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
