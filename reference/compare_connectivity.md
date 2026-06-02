# Compare measurements the connectivity of different scenarios

We can measure the connectivity of a given habitat and barrier with
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
We can also compare the connectivity, say for example if you have the
same area habitat and barrier, but you want to understand what the
change in connectedness is when you remove, or add some habitat, or some
barrier(s), or both. This function help you do that.

## Usage

``` r
compare_connectivity(area_new, area_baseline, distance, species)
```

## Arguments

- area_new:

  Numeric vector. Area of a connected patch.

- area_baseline:

  Numeric vector. Baseline area of a connected patch.

- distance:

  buffered distance

- species:

  name of species

## Value

tibble with "scenario", "distance", "species", "n_patches",
"effective_mesh_ha", and "prob_connectedness".

## Examples

``` r
# for demonstration purposes - let's imagine the area decreases by 20%
baseline_areas <- round(lizard_areas_connected$area)
new_areas <- baseline_areas[-1] * 0.8
compare_connectivity(
  area_new = new_areas,
  area_baseline = baseline_areas,
  distance = 10,
  species = "blue-tongued lizard"
)
#> # A tibble: 3 × 6
#>   scenario   distance species     n_patches effective_mesh_ha prob_connectedness
#>   <chr>         <dbl> <chr>           <int>             <dbl>              <dbl>
#> 1 baseline         10 blue-tongu…        59              4.49         0.0000171 
#> 2 new              10 blue-tongu…        58              2.87         0.0000109 
#> 3 difference       10 blue-tongu…        -1             -1.62        -0.00000618
```
