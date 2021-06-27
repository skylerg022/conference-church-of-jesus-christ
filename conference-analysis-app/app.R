library(shiny)
library(shinythemes)
library(shinyforms)
library(shinydashboard)
source('global.R')

ui <- dashboardPage(
    dashboardHeader(title = "Conference"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Speakers", tabName = "speakers", icon = icon("microphone")),
            menuItem("Trends over Time", tabName = "trends", icon = icon("chart-line")),
            menuItem("Word Cloud", tabName = "word-cloud", icon = icon("cloud"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "speakers",
                    fluidRow(
                        box(width = 6,
                            column(width = 6,
                                   selectInput('type', "Topic or word", c("Topic", "Word"), "Topic"),
                                   selectInput('speaker', 'Choose a speaker', unique_speakers, "Russell M. Nelson", selectize = TRUE),
                                   actionButton('update1', 'Update')
                            )
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            plotOutput('words'),
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "trends",
                    fluidRow(
                        box(
                            selectInput('type2', "Topic or word", c("Topic", "Word"), "Topic"),
                            conditionalPanel(
                                condition = "input.type2 == 'Topic'",
                                selectizeInput('topic_trend', 'Choose a topic',
                                               unique_topics, selected = "Jesus Christ")
                            ),
                            conditionalPanel(
                                condition = "input.type2 == 'Word'",
                                textInput('word_trend', "Choose a word", "Jesus Christ")
                            ),
                            actionButton('update2', 'Update')
                        ),
                    ),
                    fluidRow(
                        box(width = 10,
                            plotOutput('word')
                        )
                    )
            ),
            
            tabItem(tabName = "word-cloud",
                    fluidRow(
                        box(
                            numericInput('year', "Choose a year", value = 2020, min = 1971, max = 2020, step = 1),
                            selectInput('session', "Select conference", c("April", "October")),
                            actionButton('update3', 'Update')
                        )
                    ),
                    fluidRow(
                        box(width = 9,
                            plotOutput('wordcloud')
                        )    
                    )
            )
        )
    )
)


server <- function(input, output, session){
    rplot_words <- eventReactive(input$update1, {
        if(input$type == "Word"){
            plot1 <- tidy_conf_speaker %>%
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
            plot1
        } else{
            plot1 <- topic_speaker %>%
                filter(speaker == input$speaker) %>%
                top_n(7, n) %>%
                mutate(topic = reorder(topic, n)) %>%
                ggplot(aes(topic, n)) +
                geom_col(fill="#003058") +
                xlab(NULL) +
                ylab("Frequency") +
                coord_flip() +
                theme_classic() +
                theme(axis.text.y = element_text(size = 16),
                      axis.title = element_text(size = 16))
            plot1
        }
    })
    rplot_word <- eventReactive(input$update2, {
        if(input$type2 == "Word"){
            word_conf(tolower(input$word_trend), whole = FALSE, conf)
        } else {
            topic_conf(str_to_title(input$topic_trend), conf_topic)
        }
    })
    rplot_wordcloud <- eventReactive(input$update3, {
        session_num <- ifelse(input$session == "April", 4, 10)
        conf_wordcloud(input$year, session_num)
    })
    output$words <- renderPlot({ rplot_words() })
    output$word <- renderPlot({
        rplot_word()
    })
    output$wordcloud <- renderPlot({
        rplot_wordcloud()
    }, height = function() {
        session$clientData$output_wordcloud_width / 1.75})
}

shinyApp(ui = ui, server = server)
