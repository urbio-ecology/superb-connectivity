# Calculate effective mesh size

Computes the effective mesh size metric for habitat connectivity, This
represents the probability that two randomly chosen points within
habitat remain connected. Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
effective_mesh_size(area_squared, area)
```

## Arguments

- area_squared:

  Numeric vector. Squared area of connected patches.

- area:

  Numeric vector. Area of a connected patch.

## Value

Numeric. Effective mesh size in hectares.

## Examples

``` r
effective_mesh_size(lizard_areas_connected$area_squared, lizard_areas_connected$area)
#> [1] 4.491013
```
