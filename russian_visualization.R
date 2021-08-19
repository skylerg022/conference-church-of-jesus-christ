library(tidytext)
library(stopwords)
library(tidyverse)
stopwords_getsources()
stopwords_getlanguages(source = "snowball")
russian <- read.csv("conference_rus.csv")
tidy_conf <- russian %>%
  filter(!is.na(text)) %>%
  arrange(desc(year), desc(month)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_ru) %>%
  filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

russian_stop <- stopwords(language = "ru")
stop_words_ru <- data.frame(word = russian_stop, lexicon = "Snowball")

tidy_conf <- russian %>%
    filter(!is.na(text)) %>%
    arrange(desc(year), desc(month)) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words_ru) %>%
    filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

tidy_conf_speaker <- tidy_conf %>%
    group_by(speaker) %>%
    count(word, sort = TRUE) %>%
    mutate(word = str_to_title(word)) %>%
    ungroup()

unique_speakers <- sort(unique(tidy_conf$speaker))

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
x <- russian
conf_wordcloud <- function(year, session){
    library(wordcloud)
    pal <- brewer.pal(8, "Paired")
    dark_pal <- pal %>% gt::adjust_luminance(-1.0)
    tidy_conf %>% 
        filter(year == 2021, month == 4) %>%
        count(word, sort = TRUE) %>%
        with(wordcloud(word, n, random.order = TRUE, max.words = 100, colors=dark_pal, rot.per = .2, scale = c(4, .7)))
}

russian_word <- tidy_conf %>% 
    filter(year == 2021, month == 4) %>%
    count(word, sort = TRUE) %>% head(100)
wordcloud2(russian_word)
