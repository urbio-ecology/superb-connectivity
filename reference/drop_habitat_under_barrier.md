# Remove habitat under barriers

Remove habitat under barriers

## Usage

``` r
drop_habitat_under_barrier(habitat, barrier_mask)
```

## Arguments

- habitat:

  Terra SpatRaster. Habitat layer.

- barrier_mask:

  Terra SpatRaster. Barrier mask.

## Value

Terra SpatRaster with habitat remaining after barrier removal.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
barrier_mask <- create_barrier_mask(lizard_barrier)
remaining_habitat <- drop_habitat_under_barrier(
  habitat = lizard_habitat,
  barrier = lizard_barrier
  )
```
