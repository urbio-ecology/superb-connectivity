# Count number of habitat patches

Identify the number of habitat patches. A wrapper around
[`length()`](https://rdrr.io/r/base/length.html), but named to establish
its context. Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
n_patches(area_total)
```

## Arguments

- area_total:

  Numeric vector. Total areas of habitat patches.

## Value

Integer. Number of patches.

## Examples

``` r
n_patches(lizard_areas_connected$area)
#> [1] 59
```
