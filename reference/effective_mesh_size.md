# Calculate effective mesh size

Computes the effective mesh size metric for habitat connectivity, This
represents the probability that two randomly chosen points within
habitat remain connected. Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
effective_mesh_size(area, area_baseline = area)
```

## Arguments

- area:

  Numeric vector. Area of connected patches.

- area_baseline:

  Optional. Defaults to `area` if not specified. Numeric vector of
  connected patches of a baseline area. This is to allow for comparing
  the effective mesh size when comparing different scenarios. See future
  vignette on this topic (TODO).

## Value

Numeric. Effective mesh size, in hectares.

## Examples

``` r
effective_mesh_size(lizard_areas_connected$area)
#> [1] 4.473824
```
