# Extract the per-patch tables from a `connectivity` object

A `connectivity` object (from
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md)
or
[`summarise_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/summarise-connectivity.md))
carries the underlying per-patch areas in a `patch_size` list-column.
`patch_sizes()` returns them.

## Usage

``` r
patch_sizes(x)
```

## Arguments

- x:

  A `connectivity` object.

## Value

A list of
[`patch_size_tbl()`](https://urbio-ecology.github.io/urbioconnect/reference/new_patch_size_tbl.md)
objects, one per row of `x` (always a list, even for a single-row
summary).
