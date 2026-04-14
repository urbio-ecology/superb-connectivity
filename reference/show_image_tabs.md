# Display images in tabs

Helper function to display a list of image paths with tab headers in R
Markdown documents.

## Usage

``` r
show_image_tabs(images, message = NULL)
```

## Arguments

- images:

  Named character vector. Paths to image files.

- message:

  Character. Prefix message for each tab heading.

## Value

Invisible NULL. Includes images with markdown headers.

## Examples

``` r
if (FALSE) { # \dontrun{
# Typically used inside a knitr/quarto document
image_paths <- c("100m" = "plot-100m.png", "200m" = "plot-200m.png")
show_image_tabs(image_paths, message = "Buffer distance")
} # }
```
