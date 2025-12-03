# usage: col2hex("forestgreen")
col2hex <- function(color_name) {
  rgb(t(col2rgb(color_name)), maxColorValue = 255)
}

plot_barrier_habitat_buffer <- function(
  barrier,
  buffered,
  habitat,
  distance,
  species_name,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper = "white"
) {
  # First, reclassify your rasters to assign actual color values
  barrier_coloured <- subst(barrier, 1, col_barrier)
  buffer_coloured <- subst(buffered, 1, col_buffer)
  habitat_coloured <- subst(habitat, 1, col_habitat)

  # Now plot them in layers (bottom to top)
  ggplot() +
    geom_spatraster(data = buffer_coloured) +
    geom_spatraster(data = barrier_coloured) +
    geom_spatraster(data = habitat_coloured) +
    theme_minimal(paper = col_paper) +
    scale_fill_identity(na.value = NA) +
    labs(
      title = marquee_glue(
        "{.{col_habitat} {species_name} Habitat}, {.{col_buffer} {distance}m buffer}, and barrier (white)"
      )
    ) +
    theme_sub_plot(
      title = element_marquee()
    )
}

show_tabs <- function(the_list, message = NULL) {
  for (iplot in names(the_list)) {
    cat(sprintf("## %s %s\n", message, iplot))
    print(the_list[[iplot]])
    cat(sprintf("\n\n"))
  }
}

show_image_tabs <- function(images, message = NULL) {
  for (iplot in names(images)) {
    cat(sprintf("## %s %s\n", message, iplot))
    knitr::include_graphics(images[[iplot]])
    cat(sprintf("\n\n"))
  }
}

to_sentence <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_to_sentence()
}

plot_patches <- function(patch_id, distance, species_name = "Species", n_cols = 7) {
  raster_patches <- as.factor(patch_id$patch_id)

  n_patches <- patch_id$patch_id |> unique() |> nrow()

  my_colours <- qualitative_hcl(n = n_cols)

  unique_vals <- unique(values(raster_patches))
  unique_vals <- unique_vals[!is.na(unique_vals)]

  # assign colours cyclically
  colour_indices <- ((unique_vals - 1) %% n_cols) + 1
  colour_map <- my_colours[colour_indices]
  names(colour_map) <- as.character(unique_vals)

  ggplot() +
    geom_spatraster(data = raster_patches) +
    scale_fill_manual(values = colour_map, na.value = NA) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme_sub_panel(
      border = element_rect(
        colour = "grey85"
      )
    ) +
    labs(
      title = glue(
        "Patches of {species_name} habitat"
      ),
      subtitle = glue(
        "# patches: {n_patches}\nBuffer size: {distance}m\n{n_cols} colours"
      )
    )
}


plot_connectivity <- function(results_connect_habitat) {
  geo_cols <- scico(n = 6, palette = "bukavu") |> as.list()

  names(geo_cols) <- c(
    "dark_blue",
    "mid_blue",
    "light_blue",
    "dark_green",
    "tan",
    "offwhite"
  )
  results_connect_habitat |>
    dplyr::select(
      species_name:patch_area_total_ha,
      -effective_mesh_ha
    ) |>
    pivot_longer(
      cols = -c(species_name, buffer_distance)
    ) |>
    ggplot(aes(x = buffer_distance, y = value)) +
    geom_point() +
    geom_line(colour = geo_cols$dark_green) +
    facet_wrap(
      ~name,
      scales = "free",
      ncol = 2,
      labeller = labeller(name = to_sentence)
    ) +
    scale_x_continuous(
      breaks = results_connect_habitat$buffer_distance,
      labels = \(x) glue("{x}m")
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    labs(
      x = "Buffer distance (m)"
    ) +
    theme_minimal() +
    theme_sub_panel(
      border = element_rect(
        colour = "grey85",
        fill = NA
      )
    ) +
    theme(
      text = element_text(size = 14)
    )
}
