library(shiny)
library(here)
source(here("packages.R"))

ui <- fluidPage(
  titlePanel("Simple Targets + Shiny: Urbio Connectedness"),
  fluidRow(
    column(
      6,
      selectInput(
        "species",
        "Species",
        choices = c("Wood Bird", "Superb Fairy Wren"),
        width = "100%"
      ),
      sliderInput(
        "buffer_size",
        "Buffer distance (m)",
        value = 250,
        min = 0,
        max = 1000
      )
    )
  ),
  column(
    8,
    actionButton("run_pipeline", "Run Pipeline", class = "btn-primary")
  )
)

barrier_path <- function(species) {
  switch(
    species,
    "Wood Bird" = "data/wood-bird/AllWoodbirdBarriers_CorrCRS.shp",
    "Superb Fairy Wren" = "data/superb-fairy-wren/allSFWRoads.shp"
  )
}

habitat_path <- function(species) {
  switch(
    species,
    "Wood Bird" = "data/wood-bird/WoodbirdAllHabitat.shp",
    "Superb Fairy Wren" = "data/superb-fairy-wren/superbHab.shp"
  )
}

server <- function(input, output, session) {
  # Reactive value to track pipeline runs
  pipeline_run <- reactiveVal(0)

  # Run pipeline when button is clicked
  observeEvent(input$run_pipeline, {
    # Write the parameters to R/parameters.R
    writeLines(
      c(
        "# Parameters for the targets pipeline",
        "# This file is written by the Shiny app",
        "",
        glue::glue("barrier_path <- \"{barrier_path(input$species)}\""),
        glue::glue("habitat_path <- \"{habitat_path(input$species)}\""),
        glue::glue("buffer_distance_m <- {input$buffer_size}")
      ),
      "R/parameters.R"
    )

    # Run the pipeline
    # tar_make()
    pipeline_run(pipeline_run() + 1)
  })
}

shinyApp(ui, server)
