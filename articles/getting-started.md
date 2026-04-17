# Getting started

``` r
library(urbioconnect)
library(terra)
#> terra 1.9.11
```

## Overview

`urbioconnect` implements the habitat connectivity method described in
Kirk et al. (2023):

> Kirk, H., Soanes, K., Amati, M., Bekessy, S., Harrison, L., Parris,
> K., Ramalho, C., van de Ree, R., & Threlfall, C. (2023). Ecological
> connectivity as a planning tool for the conservation of wildlife in
> cities. *MethodsX*, 10, 101989.
> <https://doi.org/10.1016/j.mex.2022.101989>

The core idea is two habitat patches are considered connected if an
animal can travel between them without crossing a barrier (e.g., a
road). We buffer the habitat by a species-specific movement distance,
then cut those buffers wherever barriers exist. The remaining pieces of
habitat that are connected become **patches**, and we summarise their
areas to compute connectivity metrics. This workflow is represented
visually in the figure below, from the paper by Kirk et al.:

![](demo-effective-mesh.jpg)

This vignette walks through the raster workflow step by step using the
built-in Blue-tongued Lizard example data from Darebin Creek, Melbourne.
We then show how to run the same workflow in a single function call.

## The example data

The package ships with habitat and barrier rasters for a Blue-tongued
Lizard study area along Darebin Creek, between Preston and West
Heidelberg, Melbourne.

``` r
habitat <- example_habitat()
barrier <- example_barrier()

habitat
#> class       : SpatRaster 
#> size        : 763, 766, 1  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355) 
#> source      : lizard_habitat_raster.tif 
#> name        : Pseudo Layer 
#> min value   :            1 
#> max value   :            1
barrier
#> class       : SpatRaster 
#> size        : 763, 766, 1  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355) 
#> source      : lizard_barrier_raster.tif 
#> name        : layer 
#> min value   :     0 
#> max value   :     1
```

The habitat raster has value 1 where lizard habitat exists, NA
elsewhere.

``` r
plot(habitat, main = "Lizard Habitat", col = "seagreen", legend = FALSE)
```

![](getting-started_files/figure-html/plot-habitat-1.png)

The barrier raster has value 1 where barriers (roads exist), 0
elsewhere.

``` r
plot(barrier, main = "Lizard Barriers", col = c("white", "black"), legend = FALSE)
```

![](getting-started_files/figure-html/plot-barrier-1.png)

We can see these overlaid:

``` r
plot(barrier, main = "Lizard Habitat and Barrier", col = c("white", "black"), legend = FALSE)
plot(habitat, col = "seagreen", add = TRUE, legend = FALSE)
```

![](getting-started_files/figure-html/plot-habitat-barrier-1.png)

The dispersal distance for the blue-tongued lizard in this environment
is normally 1,000m, which suggess a buffer distance of 500 m – half the
connectivity threshold. This means two patches are considered connected
if the outside most edges are within 500 metres of each other.

While 500m is most accurate, the examples below use a smaller buffer
distance to increase computational speed.

## The pipeline, step-by-step

Let’s now break down each step in the pipeline:

1.  Create barrier mask
2.  Remove habitat under barriers
3.  Buffer the habitat
4.  Fragment habitat along barriers
5.  Assign patch IDs
6.  Summarise patch areas

### Step 1: Create the barrier mask

[`create_barrier_mask()`](https://urbio-ecology.github.io/urbioconnect/reference/create_barrier_mask.md)
converts the barrier raster (1 = barrier, NA = passable) into a
multiplier mask (NA = barrier, 1 = passable). This format lets us block
movement when we multiplying layers together.

``` r
barrier_mask <- create_barrier_mask(barrier)
plot(barrier_mask, col = c("grey"), legend = FALSE)
```

![](getting-started_files/figure-html/barrier-mask-1.png)

### Step 2: Remove habitat under barriers

Habitat sitting directly beneath a barrier is removed before analysis.
Roads bisecting a habitat patch effectively remove the habitat for
connectivity purposes. We can see here below where the habitats have
been removed - the green spaces are habitat kep, and the orange ones are
habitat that intersected with barriers.

``` r
remaining_habitat <- drop_habitat_under_barrier(
  habitat = habitat,
  barrier_mask = barrier_mask
)
```

``` r
plot(barrier_mask, col = c("grey"), legend = FALSE)
plot(habitat, col = "#d95f02", legend = FALSE, add = TRUE)
plot(remaining_habitat, col = "#1b9e77", legend = FALSE, add = TRUE)
```

![](getting-started_files/figure-html/plot-drop-under-barrier-1.png)

### Step 3: Buffer the habitat

[`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md)
expands the habitat outward using a circular focal window. The **buffer
distance** should be *half* the species’ connectivity threshold. For
example, if two patches are considered connected when within 200 m of
each other, use a 100 m buffer, as the buffers overlap when patches are
within 200 m. For the blue-tongued lizard in urban Melbourne, the median
dispersal distance is 1000 m (Kirk et al., 2023), so we would use a
buffer of 500 m. However, we use 10 for a faster demonstration on the
small example dataset.

``` r
buffer_distance <- 10

buffered_habitat <- habitat_buffer(
  habitat = remaining_habitat,
  distance = buffer_distance
)

plot(barrier_mask, col = scales::alpha("grey", 1), legend = FALSE)
plot(buffered_habitat, col = "darkgreen", legend = FALSE, add = TRUE)
plot(remaining_habitat, col = "#1b9e77", legend = FALSE, add = TRUE)
```

![](getting-started_files/figure-html/buffer-1.png)

We can now visualise the three layers together using
[`gg_barrier_habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/gg_barrier_habitat_buffer.md):

``` r
gg_barrier_habitat_buffer(
  barrier = barrier,
  buffered = buffered_habitat,
  habitat = remaining_habitat,
  distance = buffer_distance,
  species_name = "Blue-tongued Lizard",
  col_barrier = "grey30",
  col_buffer = "lightgreen",
  col_habitat = "#1b9e77",
  col_paper = "white"
)
#> <SpatRaster> resampled to 500554 cells.
#> <SpatRaster> resampled to 500554 cells.
#> <SpatRaster> resampled to 500554 cells.
```

![](getting-started_files/figure-html/plot-layers-1.png)

### Step 4: Fragment habitat along barriers

[`fragment_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/fragment_habitat.md)
cuts the buffered habitat wherever a barrier cell exists. This is the
step that disconnects patches separated by roads.

``` r
fragmented <- fragment_habitat(
  buffered_habitat = buffered_habitat,
  barrier_mask = barrier_mask
)

fragmented
#> class       : SpatRaster 
#> size        : 763, 766, 1  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355) 
#> source(s)   : memory
#> varname     : lizard_habitat_raster 
#> name        : focal_max 
#> min value   :         1 
#> max value   :         1
```

### Step 5: Assign patch IDs

[`assign_patches_to_fragments()`](https://urbio-ecology.github.io/urbioconnect/reference/assign_patches_to_fragments.md)
labels each habitat cell with an integer identifying which connected
patch it belongs to. Habitat cells that share a fragment blob get the
same patch ID.

``` r
patch_id_raster <- assign_patches_to_fragments(
  remaining_habitat = remaining_habitat,
  fragment = fragmented
) |>
  add_patch_area()

patch_id_raster
#> class       : SpatRaster 
#> size        : 763, 766, 2  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355) 
#> source(s)   : memory
#> varnames    : lizard_habitat_raster 
#>               lizard_habitat_raster 
#> names       : patch_id,     area 
#> min values  :        1, 4.000221 
#> max values  :      406, 4.000273
```

[`add_patch_area()`](https://urbio-ecology.github.io/urbioconnect/reference/add_patch_area.md)
appends a cell-area layer to the raster, which is needed for the area
calculations in the next step.

We can plot the patches, with each connected patch shown in a distinct
colour using
[`plot_patches()`](https://urbio-ecology.github.io/urbioconnect/reference/plot_patches.md):

``` r
plot_patches(
  patch_id = patch_id_raster,
  distance = buffer_distance,
  species_name = "Blue-tongued Lizard"
)
#> <SpatRaster> resampled to 500554 cells.
```

![](getting-started_files/figure-html/plot-patches-1.png)

### Step 6: Summarise patch areas

[`aggregate_connected_patches()`](https://urbio-ecology.github.io/urbioconnect/reference/aggregate_connected_patches.md)
collapses the raster down to one row per patch, summing the area of all
habitat cells within that patch.

``` r
areas_connected <- aggregate_connected_patches(patch_id_raster)
areas_connected
#> # A tibble: 163 × 3
#>    patch_id    area area_squared
#>       <dbl>   <dbl>        <dbl>
#>  1        1 97878.  9580104085. 
#>  2       15  2416.     5837832. 
#>  3       18  1304.     1700646. 
#>  4       32  1592.     2534763. 
#>  5       37     4           16.0
#>  6       39  3332.    11103470. 
#>  7       40   132.       17426. 
#>  8       44   108.       11665. 
#>  9       47    36.0       1296. 
#> 10       57  1112.     1236681. 
#> # ℹ 153 more rows
```

The result has three columns:

- `patch_id` - integer identifier for the connected patch
- `area` - total habitat area within the patch (square metres)
- `area_squared` - squared area, used in the connectivity metrics below

## Computing connectivity metrics

[`summarise_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/summarise_connectivity.md)
takes the patch area data and computes a set of landscape-level
connectivity metrics:

``` r
results <- summarise_connectivity(
  area_squared = areas_connected$area_squared,
  area_total = areas_connected$area,
  buffer_distance = buffer_distance,
  target_resolution = 500,
  data_resolution = 10,
  aggregation_factor = 50,
  species_name = "Blue-tongued Lizard"
)

results
#> # A tibble: 1 × 10
#>   species_name    buffer_distance n_patches prob_connectedness effective_mesh_ha
#>   <chr>                     <dbl>     <int>              <dbl>             <dbl>
#> 1 Blue-tongued L…              10       163           0.000017                 4
#> # ℹ 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>
```

The two key metrics are:

**Effective mesh size (ha)** - the average area of the patch a randomly
chosen animal finds itself in. Larger values mean better connectivity:

$$m_{\text{eff}} = \frac{\sum A_{i}^{2}}{\sum A_{i}} \times 0.0001$$

**Probability of connectedness** - the probability that two randomly
chosen points within habitat are in the same connected patch. Ranges
from 0 (fully fragmented) to 1 (all habitat connected):

$$P_{c} = \frac{m_{\text{eff}}}{\text{total habitat area}}$$

Where $A_{i}$ is the area of a given patch.

## Run the pipeline as a single step

We developed each step of the pipeline as a set of functions that can be
run individually. If you would like to run it all in one step, you can
run
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
Note the messages noting the step, and time taken. You can control this
by setting `verbose` to FALSE if you do not want the loading/running
messages.

``` r
areas_connected <- habitat_connectivity(
  habitat = habitat,
  barrier = barrier,
  distance = 10,
  verbose = TRUE
)
#> ℹ Creating barrier mask
#> ✔ Creating barrier mask [35ms]
#> 
#> ℹ Removing habitat underneath barrier
#> ✔ Removing habitat underneath barrier [24ms]
#> 
#> ℹ Adding buffer of 10m to habitat layer
#> ✔ Adding buffer of 10m to habitat layer [388ms]
#> 
#> ℹ Fragmenting habitat layer along barrier intersection
#> ✔ Fragmenting habitat layer along barrier intersection [21ms]
#> 
#> ℹ Assigning patches ID to fragments
#> ✔ Assigning patches ID to fragments [1.1s]
#> 
#> ℹ Summarising area in each patch
#> ✔ Summarising area in each patch [40ms]
#> 
```

The package also includes a pre-computed version of this result (at 50 m
buffer distance) as `lizard_areas_connected`:

``` r
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

If you need the intermediate rasters for mapping or reporting, use
[`habitat_connectivity_full()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity_full.md)
instead. It returns a named list containing the buffered habitat, patch
ID raster, barrier mask, remaining habitat, and the `areas_connected`
data frame:

``` r
result <- habitat_connectivity_full(
  habitat = habitat,
  barrier = barrier,
  distance = 10,
  verbose = FALSE
)

names(result)
#> [1] "buffered_habitat"  "patch_id_raster"   "areas_connected"  
#> [4] "barrier_mask"      "remaining_habitat"
```

## Comparing multiple buffer distances

A common workflow is to run the analysis at several buffer distances and
compare the results. Use
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
iterate, then
[`purrr::list_rbind()`](https://purrr.tidyverse.org/reference/list_c.html)
to combine:

``` r
buffer_distances <- c(10, 25, 50)

all_results <- purrr::map(
  buffer_distances,
  function(d) {
    areas <- habitat_connectivity(
      habitat = habitat,
      barrier = barrier,
      distance = d,
      verbose = FALSE
    )
    summarise_connectivity(
      area_squared = areas$area_squared,
      area_total = areas$area,
      buffer_distance = d,
      target_resolution = 500,
      data_resolution = 10,
      aggregation_factor = 50,
      species_name = "Blue-tongued Lizard"
    )
  }
) |>
  purrr::list_rbind()

all_results
#> # A tibble: 3 × 10
#>   species_name    buffer_distance n_patches prob_connectedness effective_mesh_ha
#>   <chr>                     <dbl>     <int>              <dbl>             <dbl>
#> 1 Blue-tongued L…              10       163           0.000017                 4
#> 2 Blue-tongued L…              25        73           0.000017                 4
#> 3 Blue-tongued L…              50        59           0.000017                 4
#> # ℹ 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>
```

With multiple buffer distances you can visualise how connectivity
changes using
[`plot_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/plot_connectivity.md):

``` r
plot_connectivity(all_results)
```

![](getting-started_files/figure-html/plot-connectivity-1.png)

[`plot_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/plot_connectivity.md)
produces a faceted ggplot2 figure showing each connectivity metric
(number of patches, probability of connectedness, mean patch area, total
habitat area) as a function of buffer distance.

## Summary

The core raster workflow in `urbioconnect` is:

1.  [`create_barrier_mask()`](https://urbio-ecology.github.io/urbioconnect/reference/create_barrier_mask.md) -
    invert the barrier raster into a passability mask
2.  [`drop_habitat_under_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/drop_habitat_under_barrier.md) -
    remove habitat beneath barriers
3.  [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md) -
    expand habitat by the movement distance (half the connectivity
    threshold)
4.  [`fragment_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/fragment_habitat.md) -
    cut the buffer where barriers exist
5.  [`assign_patches_to_fragments()`](https://urbio-ecology.github.io/urbioconnect/reference/assign_patches_to_fragments.md) +
    [`add_patch_area()`](https://urbio-ecology.github.io/urbioconnect/reference/add_patch_area.md) -
    label and measure connected patches
6.  [`aggregate_connected_patches()`](https://urbio-ecology.github.io/urbioconnect/reference/aggregate_connected_patches.md) -
    summarise patch areas
7.  [`summarise_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/summarise_connectivity.md) -
    compute effective mesh size and probability of connectedness

Or, equivalently, use
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md)
for the data frame output, or
[`habitat_connectivity_full()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity_full.md)
when you also need the intermediate rasters.
