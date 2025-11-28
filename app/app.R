library(shiny)
library(bslib)
library(DT)
library(terra)
library(sf)
library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Source UI and Server
source(here("app/ui.R"))
source(here("app/server.R"))

# Run the application
shinyApp(ui = ui, server = server)
