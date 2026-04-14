# Calculate connectivity probability

Computes the probability that two randomly chosen points within habitat
are connected, accounting for fragmentation.

## Usage

``` r
connectivity_probability(area_squared, area_total)
```

## Arguments

- area_squared:

  Numeric vector. Squared areas of connected patches.

- area_total:

  Numeric vector. Total areas of connected patches.

## Value

Numeric. Probability of connectedness (0-1).

## Examples

``` r
connectivity_probability(lizard_areas_connected$area_squared, lizard_areas_connected$area)
#> [1] 1.708751e-05
```
