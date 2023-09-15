### SHINY APP

#load libraries
library(tidyverse)
library(shiny)
library(gapminder)

#starting UI template

ui <- fluidPage(
  titlePanel(
    "Countries within continent comparasions"
  ),
  sidebarPanel(
    width = 3,
    selectizeInput(inputId = "continent",
                   label = "continent",
                   choices = unique(gapminder$continent),
                   selected = "Asia",
                   multiple = FALSE),
    
    selectizeInput(inputId = "country", 
                   label = "country",
                   choices = unique(gapminder$country),
                   selected = "",
                   multiple = TRUE)
  ),
  
  mainPanel(
    plotOutput(outputId = "lifeExp"),
    plotOutput(outputId = "pop")
  )
)


## server
#starting server template

server <- function(input, output, session){
  
  dat <- reactive({
    
    df <- gapminder %>%
      filter(continent == input$continent)
    
    updateSelectizeInput(session, "country", choices = unique(df$country))
    
    df
  })
  
  output$lifeExp <- renderPlot({
    
    dat() %>% 
      filter(country %in% input$country) %>% 
      ggplot(
        aes(
          x = year,
          y = lifeExp,
          color = country)) +
      geom_line()
    
  })
  
  output$pop <- renderPlot({
    
    dat() %>% 
      filter(country %in% input$country) %>% 
      ggplot(
        aes(x = year, 
            y = pop,
            color = country)) +
      geom_line()
    
  })
  
}


shinyApp(ui, server)
