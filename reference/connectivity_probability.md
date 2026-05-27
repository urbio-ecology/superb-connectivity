# Calculate connectivity probability

Computes the probability two randomly chosen points within habitat are
connected, accounting for fragmentation. This is given by calulating
effective mesh size (via
[`effective_mesh_size()`](https://urbio-ecology.github.io/urbioconnect/reference/effective_mesh_size.md)),
then dividing by total area. Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
connectivity_probability(area_squared, area)
```

## Arguments

- area_squared:

  Numeric vector. Squared areas of connected patches.

- area:

  Numeric vector. Area of a connected patch.

## Value

Numeric. Probability of connectedness (0-1).

## Examples

``` r
connectivity_probability(lizard_areas_connected$area_squared, lizard_areas_connected$area)
#> [1] 1.708751e-05
```
