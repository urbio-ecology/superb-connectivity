# Display plots in tabs

Helper function to display a list of plots with tab headers in R
Markdown documents.

## Usage

``` r
show_tabs(the_list, message = NULL)
```

## Arguments

- the_list:

  Named list. List of plot objects.

- message:

  Character. Prefix message for each tab heading.

## Value

Invisible NULL. Prints plots with markdown headers.

## Examples

``` r
plots <- list("100m" = ggplot2::ggplot(), "200m" = ggplot2::ggplot())
show_tabs(plots, message = "Buffer distance")
#> ## Buffer distance 100m

#> 
#> 
#> ## Buffer distance 200m

#> 
#> 
```
