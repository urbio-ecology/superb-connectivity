source(here::here("app/packages.R"))
source(here::here("app/colours.R"))
ui <- page_navbar(
  title = "Urban Connectedness",
  theme = urbio_theme(), 
  fillable = TRUE,

  # Setup Tab
  tabPanel(
    "Setup",
    h2("Setup"),
    checkboxInput(
      "use_example_data",
      "Use example data: Superb Fairy Wren",
      value = TRUE
    ),
    conditionalPanel(
      condition = "input.use_example_data",
      div(
        class = "alert alert-success",
        icon("check-circle"),
        "Using example data: Superb Fairy Wren dataset with habitat and road barriers"
      )
    ),
    textInput(
      "species_name",
      "Species Name",
      placeholder = "e.g., Superb Fairy Wren"
    ),

    # Habitat file input - show disabled version when using example data
    conditionalPanel(
      condition = "!input.use_example_data",
      fileInput(
        "habitat_file",
        "Upload Habitat Shapefile (.shp)",
        accept = ".shp"
      )
    ),
    conditionalPanel(
      condition = "input.use_example_data",
      div(
        class = "form-group shiny-input-container",
        tags$label("Upload Habitat Shapefile (.shp)"),
        tags$input(
          type = "text",
          class = "form-control",
          value = "superbHab.shp",
          disabled = "disabled",
          style = "background-color: #e9ecef; cursor: not-allowed;"
        )
      )
    ),

    # Barrier file input - show disabled version when using example data
    conditionalPanel(
      condition = "!input.use_example_data",
      fileInput(
        "barrier_file",
        "Upload Barrier Shapefile (.shp)",
        accept = ".shp"
      )
    ),
    conditionalPanel(
      condition = "input.use_example_data",
      div(
        class = "form-group shiny-input-container",
        tags$label("Upload Barrier Shapefile (.shp)"),
        tags$input(
          type = "text",
          class = "form-control",
          value = "allSFWRoads.shp",
          disabled = "disabled",
          style = "background-color: #e9ecef; cursor: not-allowed;"
        )
      )
    ),

    numericInput(
      "overlay_resolution",
      "Overlay Resolution (m)",
      value = 500,
      min = 1,
      step = 1
    ),
    numericInput(
      "base_resolution",
      "Base Resolution (m)",
      value = 10,
      min = 1
    ),
    textInput(
      "buffer_distance",
      "Buffer Distance (m)",
      value = "250",
      placeholder = "e.g., 100 or 250, 500, etc"
    ),
    actionButton("run_analysis", "Run Analysis", class = "btn-primary")
  ),

  # Results Panel
  nav_panel(
    title = "Results",
    icon = icon("chart-line"),

    # Loading indicator
    conditionalPanel(
      condition = "!output.results_ready",
      layout_column_wrap(
        card(
          card_body(
            div(
              class = "text-center p-5",
              h4("No results yet"),
              p("Upload data and run the analysis to see results here.")
            )
          )
        )
      )
    ),

    # Results content
    conditionalPanel(
      condition = "output.results_ready",

      # Analysis Metadata ----
      layout_columns(
        col_widths = 12,
        card(
          card_header(
            class = "bg-info text-white",
            "Analysis Information"
          ),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              div(
                strong("Species: "),
                textOutput("analysis_species", inline = TRUE)
              ),
              div(
                strong("Run Time: "),
                textOutput("analysis_timestamp", inline = TRUE)
              ),
              div(
                strong("Session: "),
                textOutput("analysis_session", inline = TRUE)
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              div(
                strong("Buffer Distances: "),
                textOutput("analysis_buffers", inline = TRUE)
              ),
              div(
                strong("Working Directory: "),
                code(textOutput("analysis_workdir", inline = TRUE))
              )
            )
          )
        )
      ),

      # Habitat, Buffered Habitat, and Barrier
      layout_columns(
        col_widths = 12,
        card(
          card_header("Habitat, Buffered Habitat, and Barrier"),
          card_body(
            uiOutput("plot_barrier_habitat_buffer_tabs")
          )
        )
      ),

      # Patch ID
      layout_columns(
        col_widths = 12,
        card(
          card_header("Patch ID"),
          card_body(
            uiOutput("plot_patches_tabs")
          )
        )
      ),

      # Area and patch information for each buffer
      layout_columns(
        col_widths = 12,
        card(
          card_header("Area and patch information for each buffer"),
          card_body(
            DTOutput("terra_summary_table")
          ),
          card_footer(
            downloadButton(
              "download_terra_areas_csv",
              "Download CSV",
              class = "btn-sm"
            )
          )
        )
      ),

      # Prob connectedness and summary information for each buffer
      layout_columns(
        col_widths = 12,
        card(
          card_header(
            "Prob connectedness and summary information for each buffer"
          ),
          card_body(
            DTOutput("results_connect_habitat_table")
          ),
          card_footer(
            downloadButton(
              "download_summary_csv",
              "Download CSV",
              class = "btn-sm"
            )
          )
        )
      ),

      # Longer: Prob connectedness and summary information for each buffer
      layout_columns(
        col_widths = 12,
        card(
          card_header(
            "Longer: Prob connectedness and summary information for each buffer"
          ),
          card_body(
            DTOutput("results_connect_habitat_longer_table")
          )
        )
      ),

      # Visualisation of changes in key stats over buffer distance
      layout_columns(
        col_widths = 12,
        card(
          card_header(
            "Visualisation of changes in key stats over buffer distance"
          ),
          card_body(
            plotOutput("plot_connectivity_output", height = "600px")
          ),
          card_footer(
            downloadButton(
              "download_connectivity_plot",
              "Download Plot",
              class = "btn-sm"
            )
          )
        )
      ),

      # Spatial Downloads
      layout_columns(
        col_widths = 12,
        card(
          card_header("Spatial Data Downloads"),
          card_body(
            p("Download spatial layers for use in GIS software:"),
            layout_columns(
              col_widths = c(4, 4, 4),
              downloadButton(
                "download_raster",
                "Download Raster (GeoTIFF)",
                class = "btn-outline-primary w-100"
              ),
              downloadButton(
                "download_report_html",
                "Download Report (HTML)",
                class = "btn-outline-success w-100"
              ),
              downloadButton(
                "download_report_pdf",
                "Download Report (PDF)",
                class = "btn-outline-success w-100"
              )
            )
          )
        )
      )
    )
  ),

  # About Panel
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About Urban Connectedness"),
      card_body(
        h4("Overview"),
        p(
          "This tool analyses habitat connectivity for urban planning by calculating how barriers (such as roads) fragment habitat areas."
        ),

        h4("How it works"),
        tags$ol(
          tags$li(
            "Upload habitat and barrier spatial data (shapefile or raster)"
          ),
          tags$li("Specify species name and analysis parameters"),
          tags$li(
            "Set buffer distance(s) to determine connectivity thresholds"
          ),
          tags$li(
            "View results including connectivity statistics, maps, and downloadable outputs"
          )
        ),

        h4("Outputs"),
        tags$ul(
          tags$li("Connectivity probability and effective mesh size"),
          tags$li("Number and size of connected habitat patches"),
          tags$li("Visual maps showing habitat connectivity"),
          tags$li("Downloadable spatial data and reports")
        )
      )
    )
  )
)
