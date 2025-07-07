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
library(httr)

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

getCategories <- function() {
  list(
    api_key = api_key
  ) %>%
  GET("https://api.giphy.com/v1/gifs/categories", query = .) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data")
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
            "GIPHY getEmoji" = "getEmoji",
            "GIPHY getCategories" = "getCategories"
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
          tableOutput("categoryViewer")
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
      resp <- getTrending(limit = input$limit, type = type)
      write_csv(map_dfr(resp, as_tibble), file = glue("trending_{type}.csv"))
      resp
    }
    else if (input$endpoint == "search") {
      resp <- getSearch(q = input$q, type = type)
      write_csv(map_dfr(resp, as_tibble), file = glue("search_{type}.csv"))
      resp
    }
    else if (input$endpoint == "getGIF") {
      resp <- getGIF(gif_id = input$gif_id)
      write_csv(map_dfr(resp, as_tibble), file = "getGIF.csv")
      resp
    }
    else if (input$endpoint == "getEmoji") {
      resp <- getEmoji()
      write_csv(map_dfr(resp, as_tibble), file = "getEmoji.csv")
      resp
    }
    else {
      NULL
    }
  })
  
  category_list <- eventReactive(input$get, {
    if (input$endpoint == "getCategories") {
      resp <- getCategories()
      catTibble <- tibble(
        name = map_chr(resp, "name"),
        subcategories = map_chr(resp, ~ paste(map_chr(.x$subcategories, "name"), collapse = ", ")),
        example_gif_id = map_chr(resp, ~ .x$gif$id)
      )
      write_csv(catTibble, file = "getCategories.csv")
      catTibble
    }
    else {
      NULL
    }
  })
  
  output$gifDisplay <- renderUI({
    gifs <- gif_list()
    gif_tibble <- map_dfr(gifs, as_tibble)
    req(gifs)
    
    fluidRow(
      column(
        width = 6,
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
      ),
      column(
        width = 6,
        tags$pre(
          gif_tibble %>% 
          capture.output() %>%
          paste(collapse = "\n")
        )
      )
    )
  })
  
  output$categoryViewer <- renderTable({
    category_list()
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
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)
