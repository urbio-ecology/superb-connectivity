# Calculate connectivity probability

Computes the probability two randomly chosen points within habitat are
connected, accounting for fragmentation. This requires the effective
mesh size (via
[`effective_mesh_size()`](https://urbio-ecology.github.io/urbioconnect/reference/effective_mesh_size.md)),
and the area of patches. This means that you can calculate the change in
connectivity if you calculate the effective mesh size of a new
habitat/barrier plan, and then use the baseline

## Usage

``` r
connectivity_probability(effective_mesh_size, area_baseline)
```

## Arguments

- effective_mesh_size:

  As calculated by
  [`effective_mesh_size()`](https://urbio-ecology.github.io/urbioconnect/reference/effective_mesh_size.md)

- area_baseline:

  Numeric vector. Area of a connected patch. This argument is called
  "baseline" as when you are doing design scenarios you must refer to
  the baseline area when calculating connectivity probability. See
  vignette, TODO.

## Value

Numeric. Probability of connectedness (0-1).

## Examples

``` r
effective_mesh <- effective_mesh_size(
  area = lizard_areas_connected$area
  )
connectivity_probability(
  effective_mesh_size = effective_mesh,
  area_baseline = lizard_areas_connected$area
  )
#> [1] 1.708751e-05
# if you wanted to compare to a scenario, you would consider the effective
# mesh size to be the new scenario level, and the baseline as so:
connectivity_probability(
# scenario 1
  effective_mesh_size = effective_mesh,
  area_baseline = lizard_areas_connected$area
  )
#> [1] 1.708751e-05
```
