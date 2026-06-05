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

- area_squared:

  Numeric. Squared area, used in connectivity metrics.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 59
rows and 3 columns.

## Source

Generated from
[`example_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/example-lizard-data.md)
and
[`example_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/example-lizard-data.md)
at 50 metre interpatch distance.

## See also

[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md),
[`summarise_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/summarise_connectivity.md)

## Examples

``` r
# This was the code that was run to create this object. We don't run it
# as it takes some time to run
if (FALSE) { # \dontrun{
lizard_areas_connected <- habitat_connectivity(
    habitat = example_habitat(),
    barrier = example_barrier(),
    interpatch_distance = 50,
    verbose = FALSE
  )
} # }
lizard_areas_connected
#> # A tibble: 59 × 3
#>    patch_id   area area_squared
#>       <dbl>  <dbl>        <dbl>
#>  1        1  5096.    25972178.
#>  2        3 98006.  9605178767.
#>  3        5  2416.     5837832.
#>  4        6  1304.     1700646.
#>  5        7  5008.    25083449.
#>  6        8  1112.     1236681.
#>  7        9  3276.    10733423.
#>  8       10  3232.    10447202.
#>  9       11   500.      250028.
#> 10       12  2004.     4016481.
#> # ℹ 49 more rows
```
