# Explicitly declare GitHub package dependency
options(repos = c(
  CRAN = "https://cloud.r-project.org/"
))

# This tells rsconnect where to find the package
if (!requireNamespace("urbioconnect", quietly = TRUE)) {
  remotes::install_github("urbio-ecology/superb-connectivity@shiny-mvp-i44")
}

library(urbioconnect)

source("ui.R")
source("server.R")

shinyApp(ui, server)