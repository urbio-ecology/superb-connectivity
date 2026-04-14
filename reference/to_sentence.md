# Convert snake_case to sentence case

Convert snake_case to sentence case

## Usage

``` r
to_sentence(x)
```

## Arguments

- x:

  Character vector. Text in snake_case format.

## Value

Character vector. Text converted to sentence case.

## Examples

``` r
to_sentence("prob_connectedness")
#> [1] "Prob connectedness"
to_sentence(c("n_patches", "patch_area_mean", "effective_mesh_ha"))
#> [1] "N patches"         "Patch area mean"   "Effective mesh ha"
```
