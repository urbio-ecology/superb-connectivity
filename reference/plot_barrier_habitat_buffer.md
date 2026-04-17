# Save barrier habitat buffer plot

Saved a plot created by
[`gg_barrier_habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/gg_barrier_habitat_buffer.md)
to file.

## Usage

``` r
plot_barrier_habitat_buffer(
  barrier,
  buffered,
  habitat,
  distance,
  species_name,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper
)
```

## Arguments

- barrier:

  barrier layer

- buffered:

  buffered layer

- habitat:

  habitat layer

- distance:

  buffer distance, numeric

- species_name:

  character, species name, e.g., "Superb Fairy Wren"

- col_barrier:

  colour to colour the barrier layer

- col_buffer:

  colour to colour the buffer layer

- col_habitat:

  colour to colour the habitat layer

- col_paper:

  colour to colour the paper layer of ggplot

## Value

Named character vector. The file path, named by the buffer distance.

## Examples

``` r
if (FALSE) { # \dontrun{
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
buffered <- habitat_buffer(lizard_habitat, distance = 10)
# Creates plot-barrier-buffer-habitat-*.png in the working directory
plot_barrier_habitat_buffer(
  barrier = lizard_barrier,
  buffered = buffered,
  habitat = lizard_habitat,
  distance = 10,
  species_name = "Blue-tongued Lizard",
  col_barrier = "white",
  col_buffer = "lightgreen",
  col_habitat = "seagreen",
  col_paper = "grey50"
)
} # }
```
