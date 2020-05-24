# Ideas of things to do:
# Expand to other languages (Chinese and Russian currently)
# Provide flexibility to look between April and October
# Look at topics from talks (by webscraping)
# Set up where a scripture can be inputted and talks are shown that include the scripture
# Have option to create a word cloud from a subset of data
# Pull in more data from earlier (see BYU corpus) (https://www.lds-general-conference.org/help/texts.asp)
# Do some things similar to the BYU corpus
# Named entity recognition (https://stanfordnlp.github.io/CoreNLP/ner.html?fbclid=IwAR3oQX4PdaA1m5E4XM2-5CwkEzYDTUcWNwzBJS0ouVtpGy6nTshUZOB3uAY)



#Packages used
library(tidyverse)
library(scales)
library(scriptuRs)
library(lubridate)
library(tidytext)
tail(conf$text,1)
# Get all variations of scripture citations within talks
bom_books <- unique(c(book_of_mormon$book_title, book_of_mormon$book_short_title))
ot_books <- unique(c(old_testament$book_title, old_testament$book_short_title))
nt_books <- unique(c(new_testament$book_title, new_testament$book_short_title))
pgp_books <- unique(c(pearl_of_great_price$book_title, pearl_of_great_price$book_short_title))
dc_books <- unique(c(doctrine_and_covenants$book_title, doctrine_and_covenants$book_short_title))


prep_regex <- function(book_names) {
  # Defining a reference as a citation of scripture
  reg_ex <- paste(book_names, '[0-9]+', collapse='|')
  # Correct abbreviation regex notation
  reg_ex <- str_replace_all(reg_ex, '\\.', '\\\\\\.')
  return(reg_ex)
}

conf <- read.csv('conference.csv', stringsAsFactors = FALSE)

# Function finds the citations for a book and plots a line graph
book_reference <- function(book){
  conf %>% 
  filter(!is.na(text)) %>%
  mutate(date = dmy(paste('01', month, year, sep ="/")),
         book_refs = str_count(text, prep_regex(book))) %>%
  group_by(date) %>%
  summarize(book_total = sum(book_refs)) %>%
    pivot_longer(book_total) %>%
    ggplot(aes(x=date, y=value)) +
    labs(title = paste("Citations of", book)) +
    geom_line(size=1)
} 

book_reference(c("Doctrine and Covenants"))


conf_refs <- conf %>%
  filter(!is.na(text)) %>%
  mutate(date = dmy(paste('01', month, year, sep='/')),
         bom_refs = str_count(text, prep_regex(bom_books)),
         ot_refs = str_count(text, prep_regex(ot_books)),
         nt_refs = str_count(text, prep_regex(nt_books)),
         pgp_refs = str_count(text, prep_regex(pgp_books)),
         dc_refs = str_count(text, prep_regex(dc_books))) %>%
  group_by(date) %>%
  summarize(bom_total=sum(bom_refs),
            ot_total=sum(ot_refs),
            nt_total=sum(nt_refs),
            pgp_total=sum(pgp_refs),
            dc_total=sum(dc_refs))
conf_refs %>%
  pivot_longer(bom_total:dc_total, names_to='scripture', values_to='citations') %>%
  group_by(date) %>%
  ggplot(aes(x=date, y=citations, fill=scripture, col=scripture)) +
  geom_line(size=1, span=0.75)


conf_refs %>%
  pivot_longer(bom_total:dc_total) %>%
  ggplot(aes(x=date, y=value, col=name)) +
  geom_smooth(size=1)

# Convert text to tidy table format, removing stop words
tidy_conf <- conf %>%
  filter(!is.na(text)) %>%
  arrange(desc(year), desc(month)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, '[0-9]+(:[0-9]+)*'))

# Uninteresting bar plot
tidy_conf %>%
  group_by(speaker) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(speaker == 'Russell M. Nelson') %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Creates a bar plot that shows most frequent words for a conference
# Can set the number of words using top_n instead of filtering at specific word amount
tidy_conf %>%
  group_by(year) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(year == 2020) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



# Function that McKay wrote: Finds the frequency of a word
word_conf <- function(word, whole = TRUE, data) {
  x <- data
  x$yearmonth <- round(x$year + (x$month/12), 2)
  wor <- tolower(word)
  if (whole == FALSE) x$word_count <- str_count(tolower(x$text), wor)
  if (whole == TRUE) x$word_count <- str_count(tolower(x$text), paste0("[\\s[:punct:]]",wor,"[\\s[:punct:]]"))
  
  x <- x %>% group_by(yearmonth) %>% summarize("word_count" = sum(word_count))
  return(ggplot(x, aes(x = yearmonth, y = word_count)) + geom_line() + 
           geom_vline(xintercept = pull(x[which.min(x$word_count),"yearmonth"]), color = "blue") + 
           geom_vline(xintercept = pull(x[which.max(x$word_count),"yearmonth"]), color = "red") +
           labs(x = "Year", y = "Word Count", title = paste0("Word Count by Conference: ",tools::toTitleCase(wor))))
}

word_conf("jesus christ", whole = FALSE, conf)

# Basic Shiny using uninteresting bar plot and Mckay's plot
library(shiny)
ui <- fluidPage(
  titlePanel("Common Words from Speakers"),
  sidebarLayout(
    sidebarPanel(
      textInput('speaker', "Choose a speaker", "Russell M. Nelson"),
      textInput('word', "Choose a word", "Jesus Christ"),
      selectInput('year', "Select year", selected = 2020, choices = 1971:2020)
    ),
    mainPanel(
      plotOutput('words'),
      plotOutput('word')
    )    
  )  
)

server <- function(input, output, session){
  output$words <- renderPlot({
    
    tidy_conf %>%
      group_by(speaker) %>%
      count(word, sort = TRUE) %>%
      ungroup() %>%
      filter(speaker == input$speaker) %>%
      filter(n > 150) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
  })
  output$word <- renderPlot({
    word_conf(tolower(input$word), whole = FALSE, conf)
  })
}

shinyApp(ui = ui, server = server)

## NOTE: consider factoring in number of talks into proportion calculation
frequency <- tidy_conf %>%
  count(speaker, word) %>%
  group_by(speaker) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) #%>%
  #spread(speaker, proportion) %>%
  #gather(speaker, proportion, `A. Roger Merrill`:`Yoshihiko Kikuchi`)


# expect a warning about rows with missing values being removed
frequency %>%
  filter(speaker %in% c('Russell M. Nelson', 'Spencer W. Kimball')) %>%
  spread(speaker, proportion) %>%
  ggplot(aes(x=`Russell M. Nelson`, y=`Spencer W. Kimball`, color=abs(`Russell M. Nelson` - `Spencer W. Kimball`))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = "Russel M. Nelson", y = "Spencer W. Kimball", x = NULL)

