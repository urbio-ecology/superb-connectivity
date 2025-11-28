# Urban Connectedness Shiny App

## Running the App

### Option 1: From app directory

```r
shiny::runApp("app/")
```

### Option 2: Direct execution

```r
source("app/app.R")
```

### Option 3: Using the run script

```r
source("app/shiny-run.R")
```

## App Structure

- `app.R` - Main application entry point
- `ui.R` - User interface definition using bslib
- `server.R` - Server logic and reactive components
- `non-targets-workflow.R` - workflow script based on `inst/targets/_targets.R`

## Input Requirements

### Data Files

Upload either vector (shapefile, GeoJSON) or raster (GeoTIFF) files:

- **Habitat Layer**: Spatial data representing habitat areas
- **Barrier Layer**: Spatial data representing barriers (e.g., roads)

### Parameters

- **Species Name**: Identifier for your analysis
- **Overlay Resolution**: Resolution in meters for overlay raster (default: 500)
- **Base Resolution**: Base resolution in meters (default: 10)
- **Buffer Distance(s)**: One or more distances in meters, comma-separated (e.g., "100, 250, 400")

## Outputs

### Tables

1. **Connectivity Summary**: Key metrics including:
   - Probability of connectedness
   - Effective mesh size (hectares)
   - Mean patch area
   - Total habitat area
   - Number of patches

2. **Connected Patch Areas**: Detailed patch-level information

### Visualizations

1. **Habitat and Barrier Map**: Overlay of input layers
2. **Connected Areas Map**: Patches colored by connectivity
3. **Buffer Distance Comparison**: Changes in metrics across buffer distances (if multiple buffers provided)

### Downloads

- CSV files of all tabular results
- GeoTIFF raster of habitat patches
- PNG images of all plots
- HTML/PDF reports (soon)
