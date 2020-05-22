library(tidyverse)
library(scales)
library(scriptuRs)
library(lubridate)

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

conf <- read_csv('conference.csv')

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
