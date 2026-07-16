# A set of connected habitat patches

The object returned by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md):
a [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
connected patch areas that also carries the `species` and
`interpatch_distance` the analysis was run with as attributes.

## Usage

``` r
new_patch_size(data, species, interpatch_distance, res = NA_real_)

patch_size(data, species, interpatch_distance, res = NA_real_)
```

## Arguments

- data:

  Data frame of connected patches. Must contain an `area` column.

- species:

  Character of length 1. Species the analysis was run for.

- interpatch_distance:

  Numeric of length 1. The interpatch distance (m) the analysis used.

- res:

  resolution in pixels - defaults to NA (numeric), not required for
  vector based approaches.

## Value

A `patch_size` object: a tibble with `species` and `interpatch_distance`
attributes.

## Details

Because it is a tibble subclass it behaves like a data frame directly –
`$`, `[`,
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html),
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) and
ggplot2 all work without ceremony. Read the metadata back with
[`pc_species()`](https://urbio-ecology.github.io/urbioconnect/reference/pc-getters.md)
and
[`pc_interpatch_distance()`](https://urbio-ecology.github.io/urbioconnect/reference/pc-getters.md).
