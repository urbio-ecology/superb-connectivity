# summarise_connectivity returns a tibble with expected columns

    Code
      names(result)
    Output
      [1] "species"             "interpatch_distance" "n_patches"          
      [4] "effective_mesh_ha"   "prob_connectedness"  "patch_area_mean"    
      [7] "patch_area_total_ha" "data_resolution"    

---

    Code
      result
    Output
      # A tibble: 1 x 8
        species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
        <chr>                     <dbl>     <int>             <dbl>              <dbl>
      1 Test Speci~                 100         1                 1             0.0001
      # i 3 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
      #   data_resolution <dbl>

# summarise_connectivity works with patch_connectivity data

    Code
      summarise_connectivity(lizard_areas_connected)
    Output
      # A tibble: 1 x 8
        species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
        <chr>                     <dbl>     <int>             <dbl>              <dbl>
      1 Blue-tongu~                  50        73                 4           0.000017
      # i 3 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
      #   data_resolution <chr>

---

    Code
      summarise_connectivity(connectivity = lizard_areas_connected,
        connectivity_baseline = lizard_areas_connected)
    Output
      # A tibble: 1 x 8
        species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
        <chr>                     <dbl>     <int>             <dbl>              <dbl>
      1 Blue-tongu~                  50        73                 4           0.000017
      # i 3 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
      #   data_resolution <chr>

