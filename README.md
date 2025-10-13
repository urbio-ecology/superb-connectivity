
<!-- README.md is generated from README.Rmd. Please edit that file -->

# superb-connectivity

<!-- badges: start -->

<!-- badges: end -->

The goal of superb-connectivity is to demonstrate a workflow of
connectivity analysis for the superb fairy wren. The code has been
lightly adapted from
[urbanConnect](https://github.com/urbio-ecology/urbanConnect), with the
overall method being developed by Holly Kirk et al., in [“Ecological
connectivity as a planning tool for the conservation of wildlife in
cities”](https://www.sciencedirect.com/science/article/pii/S2215016122003636?via%3Dihub).

## Comparison of use of {terra} and {raster} packages and approaches

Using `{terra}`

``` r
prepared_rasters <- terra_prepare_rasters(
  habitat = habitat,
  barrier = barrier,
  base_resolution = 10,
  overlay_resolution = 500
)
#> |---------|---------|---------|---------|==                                          

habitat_raster <- prepared_rasters$habitat_raster
barrier_raster <- prepared_rasters$barrier_raster
```

``` r
terra_areas_connected <- terra_habitat_connectivity(
  habitat = habitat_raster,
  barrier = barrier_raster,
  distance = 250
)
#> ℹ Creating barrier mask✔ Creating barrier mask [59ms]
#> ℹ Removing habitat underneath barrier✔ Removing habitat underneath barrier [16ms]
#> ℹ Adding buffer of 250m to habitat layer✔ Adding buffer of 250m to habitat layer [12.9s]
#> ℹ Fragmenting habitat layer along barrier intersection✔ Fragmenting habitat layer along barrier intersection [16ms]
#> ℹ Assigning patches ID to fragments✔ Assigning patches ID to fragments [3.7s]
#> ℹ Summarising area in each patch✔ Summarising area in each patch [373ms]

summarise_connectivity(
  area_squared = terra_areas_connected$area_squared,
  area_total = terra_areas_connected$area
)
#> # A tibble: 1 × 5
#>   n_patches prob_connectedness effective_mesh_ha patch_area_mean
#>       <int>              <dbl>             <dbl>           <dbl>
#> 1       153          0.0000227              337.          96870.
#> # ℹ 1 more variable: patch_area_total_ha <dbl>
```

Using `{raster}`

``` r
prepared_rasters <- prepare_rasters(
  habitat = habitat,
  barrier = barrier,
  base_resolution = 10,
  overlay_resolution = 500
)

habitat_raster <- prepared_rasters$habitat_raster
barrier_raster <- prepared_rasters$barrier_raster
```

(ran into issues getting this to run on render so these results are
pasted in)

``` r
# and as one step
rast_areas_connected <- rast_habitat_connectivity(
  habitat = habitat_raster,
  barrier = barrier_raster,
  distance = 250
)
```

    ✔ Creating barrier mask [207ms]
    ✔ Removing habitat underneath barrier [56ms]
    ✔ Adding buffer of 250m to habitat layer [35.5s]
    ✔ Fragmenting habitat layer along barrier intersection [64ms]
    ✔ Assigning patches ID to fragments [1.7s]
    ✔ Summarising area in each patch [22ms]
    There were 50 or more warnings (use warnings() to see the first 50)

``` r
summarise_connectivity(
  area_squared = rast_areas_connected$area_squared,
  area_total = rast_areas_connected$area
)
```

    # A tibble: 1 × 5
      n_patches prob_connectedness effective_mesh_ha patch_area_mean patch_area_total_ha
          <int>              <dbl>             <dbl>           <dbl>               <dbl>
    1       149          0.0000236              345.          98148.               1462.
