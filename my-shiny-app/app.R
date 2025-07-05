#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jsonlite)

api_key <- "L58lHADabwegckmkXK6imqqmMWF3nix4"

getTrending <- function(limit = 10) {
  list(
    api_key = api_key,
    limit = limit
  ) %>%
    GET("https://api.giphy.com/v1/gifs/trending", query = .) %>%
    content(as = "text") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height
    ))
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tab-pane {
        padding-top: 20px;
      }
    "))
  ),

  # Title
  titlePanel("GIPHY Shiny App"),

  tabsetPanel(
    tabPanel(
      "About",
      textOutput("about")
    ),
    tabPanel(
      "Data Download",
      sidebarLayout(
        sidebarPanel(
          selectInput("endpoint", "Choose an endpoint", choices = c(
            "GIPHY Trending" = "trending",
            "GIPHY Search" = "search"
          )),
          actionButton("get", "GET"),
          sliderInput("limit", "limit", min = 1, max = 50, value = 10)
        ),
        mainPanel(
          uiOutput("gifDisplay")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$about <- renderText({
    "This app uses the R package Shiny and will demonstrate my mastery of the 
    objectives taught in ST 558. Specifically, I will be querying an API and 
    summarizing the data that is returned, and I will be using shiny to build 
    an interactive web application. ... FINISH ME"
  })
  gif_list <- eventReactive(input$get, {
    print(input$endpoint)
    if (input$endpoint == "trending") {
      getTrending(limit = input$limit)
    }
    else if (input$endpoint == "search") {
      NULL
    }
  })
  
  output$gifDisplay <- renderUI({
    gifs <- gif_list()
    req(gifs)
    
    tagList(
      lapply(gifs, function(gif) {
        tags$img(
          src = gif$url,
          width = paste0(gif$width, "px"),
          height = paste0(gif$height, "px"),
          style = "margin:10px; display:block;"
        )
      })
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
