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

## urbioconnect 0.1.0

- Make a NEWS file to monitor changes.
