# enable auto-reload during development
options(shiny.autoreload = TRUE)

# run the app from the app directory
shiny::runApp(
  appDir = here::here("app"),
  launch.browser = TRUE
)
