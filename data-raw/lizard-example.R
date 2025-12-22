library(terra)
library(tidyterra)
library(tidyverse)

habitat_file <- file.path("ex/lizard_habitat.tif")
barrier_file <- file.path("ex/lizard_barrier.shp")

lizard_habitat <- terra::rast(habitat_file)

# Read shapefile and extract geometry
lizard_barrier_shp <- barrier_file |>
  sf::st_read() |>
  sf::st_geometry() |>
  sf::st_as_sf()

# rasterize barriers with explicit field value
# field = 1 means barrier cells are marked as 1 (vs 0 for background)
lizard_barrier <- terra::rasterize(
  lizard_barrier_shp,
  terra::rast(x = lizard_habitat),
  field = 1,
  background = 0
)

# coarser resolution for computational efficiency (metres)
target_resolution <- 2

# aligned grid template at target resolution
# ensures both rasters have identical cell boundaries
target_grid <- terra::rast(
  extent = terra::ext(lizard_habitat),
  resolution = target_resolution,
  crs = terra::crs(lizard_habitat)
)

# align both rasters to the target grid - using method = "near" preserves binary values (0/1)
lizard_habitat_raster <- terra::resample(
  lizard_habitat,
  target_grid,
  method = "near"
)

lizard_barrier_raster <- terra::resample(
  lizard_barrier,
  target_grid,
  method = "near"
)

plot(lizard_habitat_raster, main = "Lizard Habitat (2m resolution)")
plot(lizard_barrier_raster, main = "Lizard Barriers (2m resolution)")

# Check final resolution and dimensions
all.equal(terra::res(lizard_habitat_raster), terra::res(lizard_barrier_raster))
all.equal(dim(lizard_habitat_raster), dim(lizard_barrier_raster))
all.equal(
  terra::ncell(lizard_habitat_raster),
  terra::ncell(lizard_barrier_raster)
)


terra::writeRaster(
  x = lizard_habitat_raster,
  filename = "ex/lizard_habitat_raster.tif",
  filetype = "COG",
  overwrite = TRUE
)
terra::writeRaster(
  lizard_barrier_raster,
  "ex/lizard_barrier_raster.tif",
  filetype = "COG",
  overwrite = TRUE
)
