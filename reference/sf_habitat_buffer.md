# Buffer habitat by distance

Creates a buffer around habitat polygons and unions overlapping areas
into a single polygon.

## Usage

``` r
sf_habitat_buffer(habitat, distance)
```

## Arguments

- habitat:

  SF object. Habitat spatial data.

- distance:

  Numeric. Buffer distance in meters.

## Value

SF object with buffered and unioned habitat geometry.

## Examples

``` r
lizard_habitat_sf <- terra::as.polygons(example_habitat(), dissolve = TRUE) |>
  sf::st_as_sf()
if (FALSE) { # \dontrun{
sf_habitat_buffer(lizard_habitat_sf, distance = 10)
} # }
```
