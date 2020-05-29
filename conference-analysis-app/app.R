library(shiny)
library(shinythemes)
source('global.R')

ui <- fluidPage(
  titlePanel("Common Words from Speakers"),
  theme = shinythemes::shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      selectInput('speaker', 'Choose a speaker', unique_speakers, "Russell M. Nelson", selectize = TRUE),
      textInput('word', "Choose a word", "Jesus Christ"),
      # selectInput('year', "Select year", selected = 2020, choices = 1971:2020),
      actionButton('update', 'Update')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Speakers", plotOutput('words')),
        tabPanel("Words Over Time", plotOutput('word'))
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
