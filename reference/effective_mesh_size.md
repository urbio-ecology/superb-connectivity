# Calculate effective mesh size

Computes the effective mesh size metric for habitat connectivity, This
represents the probability that two randomly chosen points within
habitat remain connected. Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
effective_mesh_size(area_squared, area_total)
```

## Arguments

- area_squared:

  Numeric vector. Squared areas of connected patches.

- area_total:

  Numeric vector. Total areas of connected patches.

## Value

Numeric. Effective mesh size in hectares.

## Examples

``` r
effective_mesh_size(lizard_areas_connected$area_squared, lizard_areas_connected$area)
#> [1] 4.491013
```
