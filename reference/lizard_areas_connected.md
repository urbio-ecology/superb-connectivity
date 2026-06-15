# Connected habitat patch areas for Blue-tongued Lizard

Pre-computed output of
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md)
run on the lizard example data at a 50 metre interpatch distance.
Contains one row per connected habitat patch.

## Usage

``` r
lizard_areas_connected

lizard_areas_connected
```

## Format

A data frame with columns:

- patch_id:

  Integer. Connected fragment ID.

- area:

  Numeric. Total area of the connected patch in square metres.

An object of class `patch_connectivity` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 73 rows and 2 columns.

## Source

Generated from
[`example_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/example-lizard-data.md)
and
[`example_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/example-lizard-data.md)
at 50 metre interpatch distance.

## See also

[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md),
[`summarise_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/summarise-connectivity.md)

## Examples

``` r
# This was the code that was run to create this object. We don't run it
# as it takes some time to run
if (FALSE) { # \dontrun{
lizard_areas_connected <- habitat_connectivity(
    habitat = example_habitat(),
    barrier = example_barrier(),
    species = "Blue-tongued Lizard",
    interpatch_distance = 50,
    verbose = FALSE
  )
} # }
lizard_areas_connected
#> # patch_connectivity:  data.frame
#> # Species:             Blue-tongued Lizard
#> # Patches:             73
#> # Resolution:          2x2
#> # Interpatch Distance: 50 m
#>   patch_id   area
#>      <dbl>  <dbl>
#> 1        1   172.
#> 2        2  1592.
#> 3        7 98006.
#> 4        9  2416.
#> 5       10  1304.
#> # ℹ 68 more rows
```
