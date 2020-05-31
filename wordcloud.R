library(tidyverse)
library(tidytext)

conf <- read.csv('datasets/conference.csv', stringsAsFactors = FALSE)

tidy_conf <- conf %>%
    filter(!is.na(text)) %>%
    arrange(desc(year), desc(month)) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

conf_wordcloud <- function(year, session){
    library(wordcloud)
    pal <- brewer.pal(8, "Dark2")
    tidy_conf %>% 
        filter(year == year, month == session) %>%
        count(word, sort = TRUE) %>%
        with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
}
conf_wordcloud(2020, 10)
