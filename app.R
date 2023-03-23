#Setup----
library(shiny)
library(dplyr)
library(magrittr)
library(plotly)
library(miceadds)
library(shinyalert)
library(tidyverse)

options(shiny.maxRequestSize = 40*1024^2)

source("ui.R")
source("server.R")
miceadds::source.all("cosia_scripts", grepstring = ".R", print.source = FALSE)

#usethis::use_pipe(export = TRUE)

shinyApp(ui = ui, server = server)


  