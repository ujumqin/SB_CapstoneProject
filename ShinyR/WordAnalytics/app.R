#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#This app will take text and tokenize the sentence.

library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)
library(formattable)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sentence Analyzer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("sentence", helpText("Enter your text in the box below. This text will be analyzed against the bing and nrc lexicons. Additionally it only takes 1-grams, meaning each word is analyzed for sentiment by itself; context is not considered!"), 
                  value = "[Test Sentence] I like apples.", width = "4000px")),
      
      # Show data on main pane
      mainPanel(
        
        fluidRow(
          column(12, verbatimTextOutput("value")),
          
          fluidRow(
           #column(3, formattableOutput("token")),
           column(3, DT::dataTableOutput("token")),
           column(8, plotOutput("plot")),
           column(4, plotOutput("cloud")),
           column(3, plotOutput("sentiment"))
           )
          )
        )
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$value <- renderText({
    #writes the text directly to main window
    input$sentence
  }) 
  
  output$token <- DT::renderDataTable({
    #Tokenize text. 
    textbox_text <- input$sentence
    entered_text <- data.frame(textbox_text, stringsAsFactors = FALSE)
    entered_text$text <- entered_text$textbox_text
    entered_text
    
    tidy_text <- entered_text %>%
      group_by(text) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, textbox_text) %>%
      ungroup()
    
    #Analyzes text
    bing_data <- tidy_text %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) 
    
    #formatter for table
    sign_formatter <- formatter("span", 
                      style = x ~ style(color = ifelse(x == "positive", "blue", "red")))
    sign_formatter(c("positive"))
    
    #Turns into data frame so it can be further manipulated
    DT::datatable(bing_data, list(sentiment = sign_formatter))
    #formattable(bing_data, list(sentiment = sign_formatter))
  })
  
  output$plot <- renderPlot({
    textbox_text <- input$sentence
    entered_text <- data.frame(textbox_text, stringsAsFactors = FALSE)
    entered_text$text <- entered_text$textbox_text
    entered_text
    
    tidy_text <- entered_text %>%
      group_by(text) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, textbox_text) %>%
      ungroup()
    
    #Analyzes text
    bing_data <- tidy_text %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) 
    
    bing_data %>%
      group_by(sentiment) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Sentiment Words Count", x = NULL) +
      coord_flip()
  
  })  
    output$cloud <- renderPlot({
      textbox_text <- input$sentence
      entered_text <- data.frame(textbox_text, stringsAsFactors = FALSE)
      entered_text$text <- entered_text$textbox_text
      entered_text
      
      tidy_text <- entered_text %>%
        group_by(text) %>%
        mutate(linenumber = row_number()) %>%
        unnest_tokens(word, textbox_text) %>%
        ungroup()
      
      #Analyzes text
      bing_data <- tidy_text %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) 
      
    tidy_text %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 50))
  
  })  
    
    
  output$sentiment <- renderPlot({
    textbox_text <- input$sentence
    entered_text <- data.frame(textbox_text, stringsAsFactors = FALSE)
    entered_text$text <- entered_text$textbox_text
    entered_text
    
    tidy_text <- entered_text %>%
      group_by(text) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, textbox_text) %>%
      ungroup()
    
    nrc_data <- tidy_text %>%
      inner_join(get_sentiments("nrc"))
    
    ggplot(nrc_data, aes(x="", fill=sentiment)) +
      geom_bar(width = 1) +
      ggtitle("Sentiments") 
})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

