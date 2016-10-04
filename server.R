library(shiny)
library(googleVis)
library(dplyr)
library(stringr)
library(DT)
shinyServer(function(input, output,session){
  
  map_data <- reactive({
    print (as.numeric(input$Publish_Year/100))
    new %>%
      filter(century<= as.integer(input$Publish_Year/100)+1 & century>= as.integer(input$Publish_Year/100))%>%
      select(c(lon,lat, country, count))
  })
  
  map_reactive <- reactive({
    #print (map_data())
    gvisGeoChart(data = map_data(),
                 locationvar = 'country', colorvar = 'count',
                 options=list(
                   width = 'auto', height = 'auto',
                   colorAxis = color_axis
                 )
    )
  })
  
  output$map_motion <- renderGvis({

    map_reactive()
  })# end of generating map
  
  #generate interactive dataset for recommendation
  rec_data = reactive({
    data_rating %>%
      filter(period %in% input$period & type %in% input$type & country %in% input$country)%>%
      select(book.image, title)
  })
  
  
  #generate images for recommendation
  output$rec_pic1 <- renderText({
    c('<img src="',as.character(rec_data()$book.image[1]),'" width = "150" >')
  })
  output$rec_pic2 <- renderText({
    c('<img src="',as.character(rec_data()$book.image[2]),'" width = "150" >')
  })
  output$rec_pic3 <- renderText({
    c('<img src="',as.character(rec_data()$book.image[3]),'" width = "150" >')
  })
  
  
  output$tbl = renderDataTable(data_rating[,c('author','pages','publish.date','rating','title','country')]) 
 

 terms_1 <- reactive({
   withProgress({
     setProgress(message = "Processing corpus...")
     getTermMatrix(rec_data()$title[1])
     #print(rec_data()$title[1])
   })
 })
 
  wordcloud_rep <- repeatable(wordcloud)
 
 output$plot_1 = renderPlot({
   wordcloud(terms_1(), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

 })
 
 terms_2 <- reactive({
   withProgress({
     setProgress(message = "Processing corpus...")
     getTermMatrix(rec_data()$title[2])
   })
 })
 
  wordcloud_rep <- repeatable(wordcloud)
 
 output$plot_2 = renderPlot({
   wordcloud(terms_2(), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
   
 })
 
 terms_3 <- reactive({
   withProgress({
     setProgress(message = "Processing corpus...")
     getTermMatrix(rec_data()$title[3])
     #print(rec_data()$title[3])
   })
 })
 
  wordcloud_rep <- repeatable(wordcloud)
 
 output$plot_3 = renderPlot({
   wordcloud(terms_3(), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
   
 })
  
})