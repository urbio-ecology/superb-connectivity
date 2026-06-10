# Metadata from a `patch_connectivity` object

Metadata from a `patch_connectivity` object

## Usage

``` r
pc_species(x)

pc_patches(x)

pc_res(x)

pc_interpatch_distance(x)
```

## Arguments

- x:

  A
  [`patch_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/new_patch_connectivity.md)
  object.

## Value

- `pc_species()` Returns the species (character, length 1).

- `pc_interpatch_distance()` returns the interpatch distance (numeric,
  length 1).

- `pc_res()` returns the resolution (character, length 1 - e.g., "2x2").

- `pc_patches()` returns the number of patches - computed live from the
  number of rows (numeric, length 1).
