#Setup----
library(shiny)
library(shinyalert)
library(tidyverse)
library(miceadds)

options(shiny.maxRequestSize = 40*1024^2)

source("ui.R")
source("server.R")

#usethis::use_pipe(export = TRUE)

shinyApp(ui = ui, server = server)


  