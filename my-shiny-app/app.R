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
library(glue)
library(shinyjs)
library(listviewer)

api_key <- "L58lHADabwegckmkXK6imqqmMWF3nix4"

getTrending <- function(limit = 10, type = "gifs") {
  list(
    api_key = api_key,
    limit = limit
  ) %>%
    GET(glue("https://api.giphy.com/v1/{type}/trending"), query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height
    ))
}

getSearch <- function(q = "hi", limit = 10, type = "gifs") {
  list(
    api_key = api_key,
    q = q,
    limit = limit
  ) %>%
    GET(glue("https://api.giphy.com/v1/{type}/search"), query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height
    ))
}

getGIF <- function(gif_id = "xT4uQulxzV39haRFjG") {
  resp <- list(
    api_key = api_key
  ) %>%
    GET(glue("https://api.giphy.com/v1/gifs/{gif_id}"), query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data")
  
  list(list(
    url = resp$images$original$url,
    width = resp$images$original$width,
    height = resp$images$original$height
  ))
}

getCategories <- function() {
  list(
    api_key = api_key
  ) %>%
  GET("https://api.giphy.com/v1/gifs/categories", query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data")
}

getEmoji <- function(limit = 10) {
  list(
    api_key = api_key,
    limit = limit
  ) %>%
    GET("https://api.giphy.com/v2/emoji", query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height
    ))
}

ui <- fluidPage(
  useShinyjs(),
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
          radioButtons("type", "Media type:",
                       choices = c("GIFs" = "gifs", "Stickers" = "stickers"),
                       selected = "gifs",
                       inline = TRUE),
          selectInput("endpoint", "Choose an endpoint", choices = c(
            "GIPHY Trending" = "trending",
            "GIPHY Search" = "search",
            "GIPHY getGIF" = "getGIF",
            "GIPHY getCategories" = "getCategories",
            "GIPHY getEmoji" = "getEmoji"
          )),
          actionButton("get", "GET"),
          conditionalPanel(
            condition = "input.endpoint == 'trending'",
            sliderInput("limit", "limit", min = 1, max = 50, value = 10)
          ),
          conditionalPanel(
            condition = "input.endpoint == 'search'",
            textInput("q", "q", value = "hi")
          ),
          conditionalPanel(
            condition = "input.endpoint == 'getGIF'",
            textInput("gif_id", "gif_id", value = "xT4uQulxzV39haRFjG")
          )
        ),
        mainPanel(
          uiOutput("gifDisplay"),
          jsoneditOutput("categoryViewer")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$about <- renderText({
    "This app uses the R package Shiny and will demonstrate my mastery of the 
    objectives taught in ST 558. Specifically, I will be querying an API and 
    summarizing the data that is returned, and I will be using shiny to build 
    an interactive web application. ... FINISH ME"
  })
  gif_list <- eventReactive(input$get, {
    type <- input$type
    if (input$endpoint == "trending") {
      getTrending(limit = input$limit, type = type)
    }
    else if (input$endpoint == "search") {
      getSearch(q = input$q, type = type)
    }
    else if (input$endpoint == "getGIF") {
      getGIF(gif_id = input$gif_id)
    }
    else if (input$endpoint == "getEmoji") {
      getEmoji()
    }
    else {
      NULL
    }
  })
  
  category_list <- eventReactive(input$get, {
    if (input$endpoint == "getCategories") {
      getCategories()
    }
    else {
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
  
  output$categoryViewer <- renderJsonedit({
    categories <- category_list()
    req(categories)
    jsonedit(categories)
  })
  
  observe({
    if (input$endpoint == "getGIF" || input$endpoint == "getCategories"
        || input$endpoint == "getEmoji") {
      updateRadioButtons(session, "type", selected = "gifs")
      disable("type")
    }
    else {
      enable("type")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
