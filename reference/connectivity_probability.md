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
connectivity_probability(effective_mesh_size, area)
```

## Arguments

- effective_mesh_size:

  As calculated by
  [`effective_mesh_size()`](https://urbio-ecology.github.io/urbioconnect/reference/effective_mesh_size.md)

- area:

  Numeric vector. Area of a connected patch.

## Value

Numeric. Probability of connectedness (0-1).

## Examples

``` r
effective_mesh <- effective_mesh_size(
  area_squared = lizard_areas_connected$area_squared,
  area = lizard_areas_connected$area
  )
connectivity_probability(
  effective_mesh_size = effective_mesh,
  area = lizard_areas_connected$area
  )
#> [1] 1.708751e-05
# if you wanted to compare to a scenario, you would consider the effective
# mesh size to be the new scenario level, and the baseline would be "area"
connectivity_probability(
# scenario 1
  effective_mesh_size = effective_mesh,
# baseline
  area = lizard_areas_connected$area
  )
#> [1] 1.708751e-05
```
