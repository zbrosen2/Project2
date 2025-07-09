This app uses the R package Shiny and will demonstrate my mastery of the objectives taught in ST 558.  Specifically, I will be querying an API and summarizing the data that is returned, and I will be using shiny to build an interactive web application.
List of packages needed to run this application: shiny, tidyverse, jsonlite, glue, shinyjs, and httr.
These packages can be installed by the following line of code:

`
install.packages(c("shiny", "tidyverse", "jsonlite", "glue", "shinyjs", "httr"))
`

The app can then be run with the following line of code:

`
shiny::runGitHub("zbrosen2/Project2", subdir = "my-shiny-app")
`
