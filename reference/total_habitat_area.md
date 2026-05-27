# Calculate total habitat area

Calculate the total habitat area in hectars. A wrapper around summing
the area and multiplying by 0.0001 to give the units in hectares.
Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
total_habitat_area(area)
```

## Arguments

- area:

  Numeric vector. Area of a connected patch.

## Value

Numeric. Total habitat area in hectares.

## Examples

``` r
total_habitat_area(lizard_areas_connected$area)
#> [1] 26.28244
```
