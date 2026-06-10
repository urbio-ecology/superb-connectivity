# Changelog

## urbioconnect (development version)

- Use GPL (\>= 3) License.

- drop `terra_` prefix and move `rast_` functions into `scratch` where
  we test the LOO method. \* Add `sf_` prefix to vector based
  approaches.

- Add datasets and dataset loading function

- Add legend to habitat buffer barrier plot -
  [\#66](https://github.com/urbio-ecology/urbioconnect/issues/66)

- Resolve internal issue where raster might not be exactly aligned, add
  internal function `align_to()` in
  [`drop_habitat_under_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/drop_habitat_under_barrier.md),
  [`fragment_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/fragment_habitat.md),
  and
  [`assign_patches_to_fragments()`](https://urbio-ecology.github.io/urbioconnect/reference/assign_patches_to_fragments.md).

- update
  [`effective_mesh_size()`](https://urbio-ecology.github.io/urbioconnect/reference/effective_mesh_size.md)
  and
  [`connectivity_probability()`](https://urbio-ecology.github.io/urbioconnect/reference/connectivity_probability.md)
  to go from area_squared –\> area_baseline.
  [\#128](https://github.com/urbio-ecology/urbioconnect/issues/128).
  This will help facilitate
  [\#124](https://github.com/urbio-ecology/urbioconnect/issues/124).

- [`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md),
  [`habitat_connectivity_full()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity_full.md),
  and
  [`sf_habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/sf_habitat_connectivity.md)
  now take either `interpatch_distance` or `buffer_radius` (supply
  exactly one). The lower-level
  [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md)
  and
  [`sf_habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/sf_habitat_buffer.md)
  take `buffer_radius` directly.
  ([\#131](https://github.com/urbio-ecology/urbioconnect/issues/131))

- [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md)
  now warns when the buffer radius is too fine for the raster resolution
  (smaller than one cell, or not a clean multiple of it) and returns the
  habitat unchanged instead of erroring.
  ([\#131](https://github.com/urbio-ecology/urbioconnect/issues/131))

- New vignette
  [`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md)
  on how interpatch distance, buffer radius, and raster resolution
  interact.

- Fix the shiny app’s interpatch-distance input, which was read under
  the wrong id.
  ([\#131](https://github.com/urbio-ecology/urbioconnect/issues/131))

- Add “patch_connectivity” class to
  [`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md)
  function, to pave the way for attaching useful metadata to these data.
  ([\#133](https://github.com/urbio-ecology/urbioconnect/issues/133)).

- Add `patch_connectivity` S3 class:

  - add pc\_\* accessor functions to get: interpatch_distance, patches,
    res, species.
  - Extend `patch_connectivity` onto tibble
  - Add various checking functions to ensure you can compare the same
    species, and metrics together.
  - change `area` parameter for summarise/compare_connectivity to be
    `connectivity`
  - remove use of arguments, `target_resolution` and
    `aggregation_factor` from many functions as it is only really
    relevant to the spatial processing, and we really only care about
    the resolution at the end of the day

### Breaking changes

- `interpatch_distance` is now the full edge-to-edge distance below
  which two patches count as connected. It is halved internally to the
  buffer radius, so connectivity results differ from previous versions;
  reproduce old output by passing `buffer_radius =` the old value.
  ([\#131](https://github.com/urbio-ecology/urbioconnect/issues/131))

## urbioconnect 0.1.0

- Make a NEWS file to monitor changes.
