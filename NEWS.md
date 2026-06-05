# urbioconnect (development version)

* Use GPL (>= 3) License.
* drop `terra_` prefix and move `rast_` functions into `scratch` where we test the LOO method. * Add `sf_` prefix to vector based approaches.
* Add datasets and dataset loading function
* Add legend to habitat buffer barrier plot - [#66](https://github.com/urbio-ecology/urbioconnect/issues/66)
* Resolve internal issue where raster might not be exactly aligned, add internal function `align_to()` in  `drop_habitat_under_barrier()`, `fragment_habitat()`, and `assign_patches_to_fragments()`.
* update `effective_mesh_size()` and `connectivity_probability()` to go from area_squared --> area_baseline. #128. This will help facilitate #124.
* `habitat_connectivity()`, `habitat_connectivity_full()`, and `sf_habitat_connectivity()` now take either `interpatch_distance` or `buffer_radius` (supply exactly one). The lower-level `habitat_buffer()` and `sf_habitat_buffer()` take `buffer_radius` directly. (#131)

* `habitat_buffer()` now warns when the buffer radius is too fine for the raster resolution (smaller than one cell, or not a clean multiple of it) and returns the habitat unchanged instead of erroring. (#131)

* New vignette `vignette("interpatch-distance-and-resolution")` on how interpatch distance, buffer radius, and raster resolution interact.

* Fix the shiny app's interpatch-distance input, which was read under the wrong id. (#131)


## Breaking changes

* `interpatch_distance` is now the full edge-to-edge distance below which two patches count as connected. It is halved internally to the buffer radius, so connectivity results differ from previous versions; reproduce old output by passing `buffer_radius =` the old value. (#131)
 
# urbioconnect 0.1.0

* Make a NEWS file to monitor changes.
