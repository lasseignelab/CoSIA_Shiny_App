#Setup----
library(shiny)
library(dplyr)
library(magrittr)
library(plotly)
library(miceadds)
library(shinyalert)
library(tidyverse)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(org.Dm.eg.db)
library(org.Dr.eg.db)
library(org.Ce.eg.db)

options(shiny.maxRequestSize = 7*10^9)

source("ui.R")
source("server.R")

#usethis::use_pipe(export = TRUE)
shinyApp(ui = ui, server = server)


  