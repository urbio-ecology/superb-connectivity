library(ggplot2)
ggplot(penguins, aes(x = bill_len, y = bill_dep, colour = species)) +
  geom_point()

library(tidyverse)
as_tibble(penguins) |>
  group_by(species) |>
  summarise(across(where(is.numeric), \(x) sum(x)))

# fmt: table
tibble::tribble(
  ~Ozone,
  ~Solar.R,
  ~Wind,
  41L,
  190L,
  7.4,
  36L,
  118L,
  8,
  12L,
  149L,
  12.6,
  18L,
  313L,
  11.5,
  NA_integer_,
  NA_integer_,
  14.3,
  28L,
  NA_integer_,
  14.9,
)
