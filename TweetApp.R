library(shiny)
library(rtweet)
library(tmaptools)
library(leaflet)
library(tidyverse)
library(dplyr)
library(tidygeocoder)
library(tidytext)
library(textdata)
library(plotly)
library(ggplot2)
library(tidyquant)
library(wordcloud)
library(ggwordcloud)
library(shinythemes)
library(shinydashboard)
library(DT)

token <-create_token(
  app = "app",
  consumer_key= 'consumer_key',
  consumer_secret= 'consumer_secret',
  access_token = 'access_token',
  access_secret = 'access_secret',
  set_renv = TRUE
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput('element','Topic/Hashtag', placeholder = 'hashtag starts with "#" mention starts with "@"',
                value = '#Covid19'),
      sliderInput("tweet",
                  "Number of tweets:",
                  min = 50,
                  max = 3000,
                  value = 300),
      textInput('place','Location', placeholder = 'uae, dubai',value = 'usa'),
      sliderInput("loc",
                  "Twitter Search radius (miles) ",
                  min = 1,
                  max = 2000,
                  value = 600),
      
      actionButton("Get", label = 'Get', icon = icon("twitter"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      splitLayout(plotlyOutput('plot'), leafletOutput('map')),
      plotOutput('plot2'),
      downloadButton("getit", "Download tweets"),
      dataTableOutput('TAB1')
    )
  ),
  
  # Application title
  
  titlePanel("Tweet for me")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  geo_encode<-function(placeinput,alpha){
    place<-placeinput
    coord<-geo_cascade(place)
    lat<-as.numeric(coord[1])
    lan<-as.numeric(coord[2])
    boxp <- c(sw.lng = lan-alpha , 
              sw.lat = lat-alpha , 
              ne.lng = lan+alpha, 
              ne.lat = lat+alpha)
    point <- c(lat = lat, lng = lan)
    coords <- list(place = place, box = boxp, point = point)
    class(coords) <- c("coords", "list")
    return(coords)
  }
  km_deg<-function(dist){
    a<-sqrt(dist*dist/2)/111
    a
  }
  
  
  Data <- eventReactive(input$Get,{
    data<-search_tweets(q = as.character(input$element),
                        n = as.numeric(input$tweet),
                        include_rts = FALSE,
                        lang = 'en'
                        ,geocode = geo_encode(as.character(input$place), km_deg(as.numeric(input$loc)))
    )  %>%
      select(screen_name,text,coords_coords) %>%
      unnest_wider(coords_coords) %>%
      filter(!is.na(...1)) %>%
      set_names(c("screen_name", "text", "lon", "lat"))
    as.data.frame(data) 
  })
  output$map<-renderLeaflet({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    data<-Data()
    data %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(~lon,~lat,popup = as.character(data$text),label = as.character(data$screen_name)) %>%
      addCircleMarkers(~lon,~lat,weight =2, radius = input$loc/10)
    
  })
  Data2<-reactive({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    data<-Data()
    data %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments('bing'))%>%
      select(-word) %>%
      count(rowid, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
      mutate(sentiment = positive - negative) %>%
      left_join(
        data %>% select(screen_name, text) %>% rowid_to_column()
      )
  })
  output$plot2<-renderPlot({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    data<-Data()
    data %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments('bing')) %>%
      count(word, sentiment, sort =  TRUE) %>%
      slice(1:100) %>%
      mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
      ggplot(aes(label = word, color = sentiment, size = n)) +
      geom_text_wordcloud_area() +
      facet_wrap(~ sentiment, ncol = 2) +
      theme_tq() +
      scale_color_tq() + 
      scale_size_area(max_size = 16) + 
      labs(title = "Sentiment Word Frequency")
  })
  output$TAB1 <-DT::renderDataTable({
    
    req(Data2)
    data<-Data2()
    if (is.null(data())) {
      return ()
    }
    data<-Data2()
    datatable(data,filter = 'top',options = list(pageLength = 5))
  })
  output$plot<-renderPlotly({
    req(Data2)
    data<-Data2()
    if (is.null(data())) {
      return ()
    }
    data<-Data2()
    
    label_wrap <- label_wrap_gen(width = 60)
    data_formatted <- data %>%
      mutate(text_formatted =str_glue('Row ID: {rowid}
                                  Screen Name: {screen_name}
                                  Text: 
                                  {label_wrap(text)}
                                  '))
    
    g<- data_formatted %>% 
      ggplot(aes(rowid, sentiment)) +
      geom_line(color = "#2c3e50", alpha= 0.5) +
      geom_point(aes(text = text_formatted), color = "#2c3e50") +
      geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
      geom_hline(aes(yintercept = mean(sentiment)), color = 'blue') +
      geom_hline(aes(yintercept =  0.95*max(sentiment)), color = "red") +
      geom_hline(aes(yintercept =  0.95*min(sentiment)), color = "red") +
      theme_tq() +
      labs(title = "Sentiment Polarity", x = "Twitter User", y = "Sentiment")
    
    ggplotly(g, tooltips= "text") %>%
      layout(
        xaxis = list(
          rangeslider = list(type = "date")
        )
      )
  })
  
  output$getit <- downloadHandler(filename = function() {
    paste('Tweets',input$element, ".csv", sep = "")
  },content = function(file) {
    write.csv(Data2(), file, row.names = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
