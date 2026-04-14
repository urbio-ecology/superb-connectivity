# Generate Connectivity Report

Creates a parameterised Quarto report from connectivity analysis
results.

## Usage

``` r
generate_connectivity_report(
  species_name,
  buffer_distances,
  results_connect_habitat,
  areas_connected,
  habitat = NULL,
  barrier = NULL,
  habitat_raster = NULL,
  data_resolution = 10,
  target_resolution = 500,
  output_file = NULL,
  output_format = c("html", "pdf", "both"),
  output_dir = getwd()
)
```

## Arguments

- species_name:

  Character. Name of the species being analysed.

- buffer_distances:

  Numeric vector. Buffer distances used in analysis (in meters).

- results_connect_habitat:

  Data frame. Connectivity summary results.

- areas_connected:

  List of data frames. Connected patch areas for e ach buffer distance.

- habitat:

  SF object. Habitat spatial data (optional, for mapping).

- barrier:

  SF object. Barrier spatial data (optional, for mapping).

- habitat_raster:

  Terra SpatRaster. Habitat raster (optional, for mapping).

- data_resolution:

  Numeric. Data resolution in meters.

- target_resolution:

  Numeric. Target resolution in meters.

- output_file:

  Character. Output filename (without extension).

- output_format:

  Character. Output format: "html" (default), "pdf", or "both".

- output_dir:

  Character. Directory to save the report (default: current directory).

## Value

Character vector of generated report file path(s).

## Examples

``` r
if (FALSE) { # \dontrun{
report_path <- generate_connectivity_report(
  species_name = "Superb Fairy Wren",
  buffer_distances = c(100, 250, 400),
  results_connect_habitat = results_df,
  areas_connected = patches_list,
  output_format = "html"
)
} # }
```
