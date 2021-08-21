library(shiny)
library(shinythemes)
#library(shinyforms)
library(shinydashboard)
library(wordcloud2)
source('global.R')

ui <- dashboardPage(
    dashboardHeader(title = "Conference"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Intro", tabName = "intro", icon = icon("chart-line")),
            menuItem("Speakers", tabName = "speakers", icon = icon("microphone")),
            menuItem("Trends over Time", tabName = "trends", icon = icon("chart-line")),
            menuItem("Word Cloud", tabName = "word-cloud", icon = icon("cloud"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "intro",
                    fluidRow(
                        column(1),
                        box(
                        width = 10,
                        h1(strong("Text Analysis: General Conference of the Church of Jesus Christ of Latter-Day Saints")),
                        p("This app takes the text of address from the General Conference of the Church of Jesus Christ of  Latter-Day Saints from April 1971 through April 2021. There are a few features of this app that can be explored, which are explained in detail below. The purpose of this app is to take analysis and visualization of text data from the General Conference addresses and presenting in an interactive and interesting way. If there are any questions or feedback, please contact Skyler Gray, David Teuscher, or Daniel Garrett"),
                        h3(strong("Speakers:")),
                        p("The speakers tab allows a user to explore the most frequently used words by a speaker throughout all of the talks they have given between April 1971 and April 2021. The top words for each speaker is displayed in a bar chart. The user also has the option to explore the topics that a speaker has talked about most frequently. The topics were obtained from tags that were given to the talks by the Church of Jesus Christ of Latter-Saints and was not done by any of us. "),
                        h3(strong("Trends over Time")),
                        p("The trends tab allows the user to explore the frequency of a word over time between conferences session. The option is available to explore the frequency of topics over time as well"),
                        h3(strong("Word Cloud:")),
                        p("A word cloud can be created for the most frequent words during a conference session from April 1971 until April 2021")
                    ))
                    ),
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
                            numericInput('year', "Choose a year", value = 2021, min = 1971, max = 2021, step = 1),
                            selectInput('session', "Select conference", c("April", "October")),
                            actionButton('update3', 'Update')
                        )
                    ),
                    fluidRow(
                        box(width = 9,
                            wordcloud2Output('wordcloud')
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
    output$wordcloud <- renderWordcloud2({
        rplot_wordcloud()
    })
}

shinyApp(ui = ui, server = server)