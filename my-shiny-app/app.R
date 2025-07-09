# Author: Zachary Rosen
# Date: 7/8/25

library(shiny)
library(tidyverse)
library(jsonlite)
library(glue)
library(shinyjs)
library(httr)

api_key <- "L58lHADabwegckmkXK6imqqmMWF3nix4"

# functions that call web API endpoints (GIPHY)

# trending endpoint with limit query param
# returns trending gifs/stickers
getTrending <- function(limit = 10, type = "gifs") {
  list(
    api_key = api_key,
    limit = limit
  ) %>%
    # glue type to URI (gif or sticker)
    GET(glue("https://api.giphy.com/v1/{type}/trending"), query = .) %>%
    # format response and pluck data
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    # extract original image properties from each object
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height,
      size = .x$images$original$size
    ))
}

# search endpoint with q (query) and limit query params
# returns gifs/stickers that match q (query)
getSearch <- function(q = "hi", limit = 10, type = "gifs") {
  list(
    api_key = api_key,
    q = q,
    limit = limit
  ) %>%
    GET(glue("https://api.giphy.com/v1/{type}/search"), query = .) %>%
    # format response and pluck data
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    # extract original image properties from each object
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height,
      size = .x$images$original$size
    ))
}

# get gif by id endpoint with gif_id query param
# returns gif that corresponds to provided id
getGIF <- function(gif_id = "xT4uQulxzV39haRFjG") {
  resp <- list(
    api_key = api_key
  ) %>%
    GET(glue("https://api.giphy.com/v1/gifs/{gif_id}"), query = .)
  
  # error handling if gif_id does not exist
  if (http_error(resp)) {
    body <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(simplifyVector = TRUE)
    
    status <- body$meta$status
    msg <- body$meta$msg
    error_detail <- body$meta$error_code
    
    showNotification(glue("HTTP Status {status} {msg}: {error_detail}"), type = "error")
    return(NULL)
  }
  
  # format response and pluck data
  resp <- resp %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data")
  
  # extract original image properties from object
  list(list(
    url = resp$images$original$url,
    width = resp$images$original$width,
    height = resp$images$original$height,
    size = resp$images$original$size
  ))
}

# get emojis endpoint with limit query param
# returns emojis
getEmoji <- function(limit = 10) {
  list(
    api_key = api_key,
    limit = limit
  ) %>%
    GET("https://api.giphy.com/v2/emoji", query = .) %>%
    # format response and pluck data
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data") %>%
    # extract original image properties from each object
    map(~ list(
      url = .x$images$original$url,
      width = .x$images$original$width,
      height = .x$images$original$height,
      size = .x$images$original$size
    ))
}

# categories endpoint
# returns categories, subcategories, and an example gif that belongs to category
getCategories <- function() {
  list(
    api_key = api_key
  ) %>%
  GET("https://api.giphy.com/v1/gifs/categories", query = .) %>%
    # format response and pluck data
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(simplifyVector = FALSE) %>%
    pluck("data")
}

# define ui
# start with fluid page and use shiny js
ui <- fluidPage(
  useShinyjs(),
  # add global padding to top
  tags$head(
    tags$style(HTML("
      .tab-pane {
        padding-top: 20px;
      }
    "))
  ),

  # title
  titlePanel("GIPHY Shiny App"),

  # define tabset
  tabsetPanel(
    # about tab
    tabPanel(
      "About",
      uiOutput("about")
    ),
    # data download tab
    tabPanel(
      "Data Download",
      # define sidebar
      sidebarLayout(
        sidebarPanel(
          # radio buttons to select media type
          radioButtons("type", "Media type:",
                       choices = c("GIFs" = "gifs", "Stickers" = "stickers"),
                       selected = "gifs",
                       inline = TRUE),
          # endpoint selector
          selectInput("endpoint", "Choose an endpoint", choices = c(
            "GIPHY Trending" = "trending",
            "GIPHY Search" = "search",
            "GIPHY getGIF" = "getGIF",
            "GIPHY getEmoji" = "getEmoji",
            "GIPHY getCategories" = "getCategories"
          )),
          # define GET button (clicking sends an HTTP GET request to corresponding endpoint)
          actionButton("get", "GET"),
          # provide limit query param slider for corresponding endpoints
          conditionalPanel(
            condition = "input.endpoint == 'trending' || input.endpoint == 'search'
                        || input.endpoint == 'getEmoji'",
            sliderInput("limit", "limit", min = 1, max = 50, value = 10)
          ),
          # provide text input for search endpoint with 'hi' as default query
          conditionalPanel(
            condition = "input.endpoint == 'search'",
            textInput("q", "q", value = "hi")
          ),
          # provide text input for get gif by id endpoint with default
          conditionalPanel(
            condition = "input.endpoint == 'getGIF'",
            textInput("gif_id", "gif_id", value = "xT4uQulxzV39haRFjG")
          )
        ),
        # output gifs/stickers/emojis and category table when server asks for it
        mainPanel(
          uiOutput("gifDisplay"),
          tableOutput("categoryViewer")
        )
      )
    ),
    # data exploration tab
    tabPanel(
      "Data Exploration",
      sidebarLayout(
        # reset data button
        sidebarPanel(
          div(
            actionButton("resetData", "Reset All Retrieved Data"),
            style = "margin-bottom: 20px;"
          ),
          # output variable selection when server asks for it
          uiOutput("variableSelect"),
          # summary type input
          selectInput("summaryType", "Summary Type:",
                      choices = c("Numerical Summary", "Contingency Table")),
          conditionalPanel(
            condition = "input.summaryType == 'Numerical Summary'",
            # plot type input
            selectInput("plotType", "Plot Type:",
                        choices = c("Bar Plot", "Box Plot", "Scatter Plot", "Heatmap")),
            # facet toggle
            checkboxInput("facetToggle", "Facet by second variable", value = FALSE),
            # output facet variable UI when server asks for it
            uiOutput("facetVarUI")
          )
        ),
        mainPanel(
          # output data warning when server asks for it
          uiOutput("dataWarning"),
          # output summary and plot when server asks for it
          uiOutput("summaryAndPlot")
        )
      )
    )
  )
)

# define server
server <- function(input, output, session) {
  # define reactive cumulative data value
  cumulative_data <- reactiveVal(tibble())
  
  # render about text
  output$about <- renderUI({
    tagList(
      tags$p("This app uses the R package Shiny and will demonstrate my mastery of the 
              objectives taught in ST 558. Specifically, I will be querying an API and 
              summarizing the data that is returned, and I will be using shiny to build 
              an interactive web application. The web API I will be using is GIPHY, which
              hosts many GIFs, Stickers, and Emojis. You can find out more about GIPHY
              here: https://developers.giphy.com/. The Data Download tab allows you to
              view GIFs, Stickers, and Emojis that you can retrieve using the web API's
              various endpoints. You can also see the categories that GIPHY uses. Pressing
              'GET' also downloads the endpoint's response into a csv file. Pressing
              'GET' also adds the data to the Data Exploration tab. The Data Exploration tab
              allows you to explore the data through plots, numerical summaries, and contingency
              tables. You can customize plot type, summary type, and the variables you want to be
              considered. You can also add another variable to facet onto a plot"),
      tags$img(
        src = "https://ucb35fc568502c0aeab164608c2e.previews.dropboxusercontent.com/p/thumb/ACvoxGnw_ZRp3kVKW6_ni0TK-t13XCWzsacaP0DyQhkNxGpfrXt4bsiOTosQEx5_qxL6yfuSYJXj1Ph3v5HLv4DmGabk-D2bFOM8LiA3UKf_fVwev7DiyqnBCrfaMvKEIffyRo5FffFpz8WoVZsRG_PQM2MtPnR2vejRZIWH_340FQo1nO1sa2atW_RTirhp6rIJoX5PZ01rpQoZq1Qd1YJTSO_8tdy4CyT3PJew1L7i2nP_R8p1c7_iZAeh0RrfMhJfejlyNtKC1I6iDJpGoyfQ6_FZpNK5d67oXwLe9aDkkpTD-hOt6u-UBVrTQ7--ptje_MzhSitRtKbcGIq0idLK/p.png"
      )
    )
  })
  
  # render data warning
  output$dataWarning <- renderUI({
    if (nrow(cumulative_data()) == 0) {
      tags$p("Please start by selecting an endpoint and clicking 'GET' in the Data Download tab.
             Data will be cumulatively added as 'GET' is clicked.
             Note that the 'categories' endpoint does not affect this tab.")
    }
  })
  
  # render summary and plot
  output$summaryAndPlot <- renderUI({
    if (nrow(cumulative_data()) != 0) {
      if (input$summaryType == "Numerical Summary") {
        fluidRow(
          column(
            width = 6,
            tags$h4("Summary"),
            tableOutput("summaryTable")
          ),
          column(
            width = 6,
            tags$h4("Plot"),
            plotOutput("explorePlot")
          )
        )
      } else {
        fluidRow(
          column(
            width = 12,
            tags$h4("Contingency Table"),
            tableOutput("summaryTable")
          )
        )
      }
    }
  })
  
  # when GET button is pressed (logic for all endpoints but category endpoint)
  gif_list <- eventReactive(input$get, {
    # pass type (gifs, stickers, or emojis)
    type <- input$type
    
    # call endpoint functions when those endpoints are selected by user
    # download data to csv
    if (input$endpoint == "trending") {
      resp <- getTrending(limit = input$limit, type = type)
      write_csv(map_dfr(resp, as_tibble), file = glue("trending_{type}.csv"))
    }
    else if (input$endpoint == "search") {
      resp <- getSearch(q = input$q, limit = input$limit, type = type)
      write_csv(map_dfr(resp, as_tibble), file = glue("search_{type}.csv"))
    }
    else if (input$endpoint == "getGIF") {
      # gsub for data cleansing (remove spaces)
      resp <- getGIF(gif_id = gsub(" ", "", input$gif_id))
      write_csv(map_dfr(resp, as_tibble), file = "getGIF.csv")
    }
    else if (input$endpoint == "getEmoji") {
      resp <- getEmoji(limit = input$limit)
      write_csv(map_dfr(resp, as_tibble), file = "getEmoji.csv")
    }
    else {
      return(NULL)
    }
    
    # define new data to add to cumulative data variable
    new_data <- map_dfr(resp, as_tibble) %>%
      # allow emoji type if emoji endpoint is used
      mutate(type = if (input$endpoint == "getEmoji") "emoji" else input$type)
    
    # get current cumulative data and bind new data
    current <- cumulative_data()
    cumulative_data(bind_rows(current, new_data))
    
    return(resp)
  })
  
  # when GET button is pressed (logic for category endpoint)
  category_list <- eventReactive(input$get, {
    if (input$endpoint == "getCategories") {
      # call category function
      resp <- getCategories()
      # map category name, subcategories, and example gif id to tibble columns
      catTibble <- tibble(
        name = map_chr(resp, "name"),
        subcategories = map_chr(resp, ~ paste(map_chr(.x$subcategories, "name"), collapse = ", ")),
        example_gif_id = map_chr(resp, ~ .x$gif$id)
      )
      # download data to csv
      write_csv(catTibble, file = "getCategories.csv")
      catTibble
    }
    else {
      NULL
    }
  })
  
  # data explore reactive function (because it depends on reactive cumulative data)
  data_explore <- reactive({
    # get cumulative data
    gifs <- cumulative_data()
    req(nrow(gifs) > 0)
    
    # convert dimensions and size to numeric and categorize width and height into bins
    gifs %>%
      mutate(
        width = as.numeric(width),
        height = as.numeric(height),
        size = as.numeric(size),
        width_bin = cut(width, breaks = 3, labels = c("Small", "Medium", "Large")),
        height_bin = cut(height, breaks = 3, labels = c("Short", "Medium", "Tall"))
      )
  })
  
  # render gifs
  output$gifDisplay <- renderUI({
    # get gifs and convert to single tibble
    gifs <- gif_list()
    gif_tibble <- map_dfr(gifs, as_tibble)
    req(gifs)
    
    # handle no gifs found case
    if (length(gifs) == 0) {
      return(tags$p("No GIFs found.  Please try a different query."))
    }
    
    # define column for rendering gifs
    fluidRow(
      column(
        width = 6,
        tagList(
          # loop over each gif and render img tag with inline styling
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
      # define column for tibble tibble output (endpoint response data)
      column(
        width = 6,
        # format and display response data (shorten url, extract id, and reorder columns)
        tags$pre(
          gif_tibble %>% 
            mutate(
              id = sapply(strsplit(url, "/"), function(x) x[6]),
              url = paste0(substr(url, 1, 20), "...")
            ) %>%
            select(id, everything()) %>%
          capture.output() %>%
          paste(collapse = "\n")
        )
      )
    )
  })
  
  # render category list as table
  output$categoryViewer <- renderTable({
    req(category_list())
    category_list()
  })
  
  # render variable selection UI
  output$variableSelect <- renderUI({
    # get data for exploration
    gifsTibble <- data_explore()
    req(gifsTibble)
    
    catVars <- c("type", "width_bin", "height_bin")
    numVars <- c("width", "height", "size")
    
    # ensure categorical variables for contingency table
    if (input$summaryType == "Contingency Table") {
      tagList(
        selectInput("xVar", "X Variable", choices = catVars),
        selectInput("yVar", "Y Variable", choices = catVars)
      )
    } else {
      tagList(
        selectInput("xVar", "X Variable", choices = catVars),
        selectInput("yVar", "Y Variable", choices = numVars)
      )
    }
  })
  
  # render facet variable UI
  output$facetVarUI <- renderUI({
    req(input$facetToggle)
    # get data for exploration
    gifsTibble <- data_explore()
    # input selector for facet variable
    selectInput("facetVariable", "Facet Variable", choices = c("type", "width_bin", "height_bin"))
  })
  
  # render summary table
  output$summaryTable <- renderTable({
    # get data for exploration
    gifsTibble <- data_explore()
    req(input$xVar, input$yVar)
    
    # if numerical summary is selected
    if (input$summaryType == "Numerical Summary") {
      
      # make sure y var is numeric
      if (!is.numeric(gifsTibble[[input$yVar]])) {
        return(NULL)
      }
      
      # group by x var
      gifsTibble %>%
        group_by(.data[[input$xVar]]) %>%
        # data summary on y var (mean and standard deviation)
        summarise(
          summary_variable = input$yVar,
          mean = mean(.data[[input$yVar]], na.rm = TRUE),
          sd = sd(.data[[input$yVar]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    # otherwise render contingency table
    else {
      validCats <- c("type", "width_bin", "height_bin")
      req(input$xVar %in% validCats, input$yVar %in% validCats)
      
      table <- table(gifsTibble[[input$xVar]], gifsTibble[[input$yVar]])
      
      contingency_df <- as.data.frame(table)
      colnames(contingency_df)[1:2] <- c(input$xVar, input$yVar)
      
      contingency_df
    }
  })
  
  # render plot
  output$explorePlot <- renderPlot({
    # get data for exploration
    gifsTibble <- data_explore()
    req(input$xVar, input$yVar)
    
    # base plot object
    p <- ggplot(gifsTibble, aes(x = .data[[input$xVar]], y = .data[[input$yVar]]))
    
    # bar plot
    if (input$plotType == "Bar Plot") {
      p <- p + geom_bar(stat = "identity", fill = "steelblue")
    # box plot
    } else if (input$plotType == "Box Plot") {
      p <- p + geom_boxplot(aes(fill = .data[[input$xVar]])) +
        theme(legend.position = "none")
    # scatter plot
    } else if (input$plotType == "Scatter Plot") {
      p <- p + geom_point(aes(color = .data[[input$xVar]]), alpha = 0.6)
    # heat map
    } else if (input$plotType == "Heatmap") {
      p <- ggplot(gifsTibble, aes(x = .data[[input$xVar]], y = .data[[input$yVar]])) +
        geom_bin2d() +
        scale_fill_viridis_c()
    }
    
    # if facet toggle is selected and the facet variable exists
    if (input$facetToggle && !is.null(input$facetVariable)) {
      # add facet variable to plot
      p <- p + facet_wrap(vars(.data[[input$facetVariable]]))
    }
    
    # add title and x and y labels
    p + labs(
      title = paste(input$plotType, "of", input$yVar, "vs", input$xVar),
      x = input$xVar,
      y = input$yVar
    ) +
    theme_minimal()
  })
  
  # if get gif by id, category, or get emojis endpoint is selected then
  # disable type radio button (only search and trending endpoints allow type toggling)
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
  
  # if reset data button is pressed then reset cumulative data variable
  observeEvent(input$resetData, {
    cumulative_data(tibble())
  })
}

# run the application 
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)
