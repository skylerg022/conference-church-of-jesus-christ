library(shiny)
library(shinythemes)
source('global.R')


# This could be used to develop a form for people to fill out to get feedback

#devtools::install_github("daattali/shinyforms") Use this to install shinyforms
# https://github.com/daattali/shinyforms
#questions <- list(
#  list(id = "name", type = "text", title = "Name", mandatory = TRUE),
#  list(id = "email", type = "text", title = "Email", mandatory = FALSE),
#  list(id = "positive", type = "text", title = "What do you like about the app?", mandatory = FALSE),
#  list(id = "negative", type = "text", title = "What do you not like about the app?", mandatory = FALSE),
#  list(id = "ideas", type = "text", title = "What ideas do you have for improvement of this app?", mandatory = FALSE)
#)

ui <- navbarPage("General Conference",
      theme = shinythemes::shinytheme("flatly"),
      tabPanel("Speakers",
        sidebarLayout(
          sidebarPanel(
            selectInput('speaker', 'Choose a speaker', unique_speakers, "Russell M. Nelson", selectize = TRUE),
            actionButton('update1', 'Update')
          ),
          mainPanel(
            plotOutput('words')
          )
        )
      ),
      tabPanel("Words Over Time",
        sidebarLayout(
          sidebarPanel(
            textInput('word', "Choose a word", "Jesus Christ"),
              actionButton('update2', 'Update')
          ),
          mainPanel(
            plotOutput('word')
          )
        )  
      )    
)

server <- function(input, output, session){
  rplot_words <- eventReactive(input$update1, {
    tidy_conf_speaker %>%
      filter(speaker == input$speaker) %>%
      top_n(15, n) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill="#003058") +
      xlab(NULL) +
      ylab("Frequency") +
      coord_flip() +
      theme_classic() +
      theme(axis.text.y = element_text(size = 16),
            axis.title = element_text(size = 16))
  })
  rplot_word <- eventReactive(input$update2, {
    word_conf(tolower(input$word), whole = FALSE, conf)
  })
  output$words <- renderPlot({ rplot_words() })
  output$word <- renderPlot({
    rplot_word()
  })
}

shinyApp(ui = ui, server = server)

