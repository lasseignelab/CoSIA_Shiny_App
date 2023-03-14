#Setup----
library(shiny)
library(shinyalert)
library(plotly)
library(tidyverse)
library(miceadds)
library(magrittr)
options(shiny.maxRequestSize = 40*1024^2)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)


  