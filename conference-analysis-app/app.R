library(shiny)
library(shinythemes)
source('global.R')

ui <- navbarPage("General Conference",
      theme = shinythemes::shinytheme("cerulean"),
      tabPanel("Speakers",
        sidebarLayout(
          sidebarPanel(
            selectInput('speaker', 'Choose a speaker', unique_speakers, "Russell M. Nelson", selectize = TRUE),
            actionButton('update', 'Update')
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
              actionButton('update', 'Update')
          ),
          mainPanel(
            plotOutput('word')
          )
        )  
      )    
)  

server <- function(input, output, session){
  rplot_words <- eventReactive(input$update, {
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
  rplot_word <- eventReactive(input$update, {
    word_conf(tolower(input$word), whole = FALSE, conf)
  })
  output$words <- renderPlot({ rplot_words() })
  output$word <- renderPlot({
    rplot_word()
  })
}

shinyApp(ui = ui, server = server)
