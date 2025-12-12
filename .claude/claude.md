# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About This Project

This is a shiny app for people in government doing urban planning called "Urban Connectedness".

- Use material, modern themes
- Use modern bootstrap, bslib, cards, etc
- Make it easy to use, show loading screens when uploading data

Inputs and features:

- Habitat Raster (or vector) data
- Barrier Raster (or vector) data 
- Buffer distance specification (can be more than 1 number)
- Input from the user: `species_name <- "Superb Fairy Wren"`
- input from the user: `overlay_resolution <- 500`
- input from the user: `base_resolution <- 10`
- input from the user - can be one number, up to 4 numbers: `buffer_distance <- c(100, 250, 400)`

They then get as outputs

- A "results" page/tab, that will have the following
	- areas_connected - a data table of results
	- `results_connect_habitat` another data table of results
	- placeholders for
		- Output map of habitat, buffer, and barrier
		- Output map showing habitat by connected area (patches)
		- And spatial file that generates that map (e.g., geotiff/raster etc) so they can use that in their GIS
		- Area and patch information for each buffer (data table), As a CSV
		- Prob connectedness and summary information for each buffer
		- As the DataTable, As a CSV
		- Visualisation of changes in key stats over buffer distance (Only show this if people put more than one buffer in there). 
- A single report (PDF / HTML) showing all these things as well
- All key pieces should be able to be downloaded

All key code is in `app/non-targets-workflow.R` and the R directory.


## Key development commands

General advice:
* Use Australian (British) English spelling
* When running R from the console, always run it with `--quiet --vanilla`
* Always run `air format .` after generating code
* Only make changes to R code for ui and server in app/ui.R and app/server.R

### Testing

- DO NOT USE `devtools::test_active_file()`
- All testing functions automatically load code; you don't needs to.

### Documentation to read

You should read:

- https://mastering-shiny.org/

## Core Architecture

### File Organization

- `R/` - All R source code, organized by functionality`
- Starting code is in app/app.R
