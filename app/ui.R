library(shiny)
library(bslib)
library(DT)

ui <- page_navbar(
  title = "Urban Connectedness",
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#007bff",
    secondary = "#6c757d",
    success = "#28a745",
    base_font = font_google("Inter"),
    heading_font = font_google("Roboto")
  ),
  fillable = TRUE,

  ## inputs ----
  nav_panel(
    title = "Setup",
    icon = icon("gear"),
    layout_column_wrap(
      col_widths = c(6, 6),

      # Left Column - File Uploads
      card(
        card_header("Upload your data"),
        card_body(
          fileInput(
            "habitat_file",
            "Upload Habitat Data",
            accept = c(
              ".shp",
              ".shx",
              ".dbf",
              ".prj",
              ".tif",
              ".tiff",
              ".geojson"
            ),
            multiple = TRUE
          ),
          fileInput(
            "barrier_file",
            "Upload Barrier Data",
            accept = c(
              ".shp",
              ".shx",
              ".dbf",
              ".prj",
              ".tif",
              ".tiff",
              ".geojson"
            ),
            multiple = TRUE
          ),
          p(
            class = "text-muted small",
            
            HTML(
              "Leave files blank to use example data<br>
              <strong>Shapefile:</strong> Select all files (.shp, .shx, .dbf, .prj)<br>
                  <strong>Raster:</strong> Select .tif or .tiff file<br>
                  <strong>Vector:</strong> Select .geojson file"
            )
          ),
          textInput(
            "species_name",
            "Species Name",
            value = "Superb Fairy Wren",
            placeholder = "Species of interest, e.g., Superb Fairy Wren"
          )
        )
      ),

      # Right Column - Parameters
      card(
        card_header("Analysis Parameters"),
        card_body(
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
            min = 1,
            step = 1
          ),
          textInput(
            "buffer_distance",
            "Buffer Distance(s) (m)",
            value = "100",
            placeholder = "e.g., 100 or 100, 250, 400"
          ),
          p(
            class = "text-muted small",
            "Enter one or more buffer distances separated by commas"
          )
        )
      )
    ),

    # Action Button
    layout_columns(
      col_widths = 12,
      card(
        card_body(
          actionButton(
            "run_analysis",
            "Run Analysis",
            icon = icon("play"),
            class = "btn-primary btn-lg w-100"
          )
        )
      )
    )
  ),

  # Results Panel ----
  nav_panel(
    title = "Results",
    icon = icon("chart-line"),

    # Loading indicator
    conditionalPanel(
      condition = "!output.results_ready",
      layout_column_wrap(
        # col_widths = 12,
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

      # Summary Statistics
      layout_column_wrap(
        # col_widths = 12,
        card(
          card_header("Connectivity Summary"),
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

      # Patch Areas
      layout_columns(
        col_widths = 12,
        card(
          card_header("Connected Patch Areas"),
          card_body(
            DTOutput("terra_areas_connected_table")
          ),
          card_footer(
            downloadButton(
              "download_patches_csv",
              "Download CSV",
              class = "btn-sm"
            )
          )
        )
      ),

      # Maps
      layout_column_wrap(
        card(
          card_header("Habitat, Barrier & Buffer Map"),
          card_body(
            plotOutput("map_habitat_barrier", height = "400px")
          ),
          card_footer(
            downloadButton("download_map_1", "Download Plot", class = "btn-sm")
          )
        ),
        card(
          card_header("Connected Areas Map"),
          card_body(
            plotOutput("map_patches", height = "400px")
          ),
          card_footer(
            downloadButton("download_map_2", "Download Plot", class = "btn-sm")
          )
        )
      ),

      # Buffer Distance Comparison (conditional)
      conditionalPanel(
        condition = "output.show_buffer_comparison",
        layout_columns(
          col_widths = 12,
          card(
            card_header("Buffer Distance Comparison"),
            card_body(
              plotOutput("plot_buffer_comparison", height = "500px")
            ),
            card_footer(
              downloadButton(
                "download_comparison_plot",
                "Download Plot",
                class = "btn-sm"
              )
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

  # About Panel ----
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About Urban Connectedness"),
      card_body(
        h4("Overview"),
        p(
          "This tool analyzes habitat connectivity for urban planning by calculating how barriers (such as roads) fragment habitat areas."
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
