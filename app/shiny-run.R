library(here)

# Enable auto-reload during development
options(shiny.autoreload = TRUE)

# Run the app from the app directory
shiny::runApp(
  appDir = here("app"),
  launch.browser = TRUE
)
