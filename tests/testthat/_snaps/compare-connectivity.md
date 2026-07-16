# compare_connectivity works for patch_size

    Code
      compare_connectivity(connectivity = lizard_areas_connected,
        connectivity_baseline = lizard_areas_connected)
    Output
      # A tibble: 3 x 6
        scenario interpatch_distance species             n_patches effective_mesh_ha
        <chr>                  <dbl> <chr>                   <int>             <dbl>
      1 baseline                  50 Blue-tongued Lizard        73              4.47
      2 new                       50 Blue-tongued Lizard        73              4.47
      3 change                    50 Blue-tongued Lizard         0              0   
      # i 1 more variable: prob_connectedness <dbl>

# compare-connectivity works

    Code
      compare_connectivity(connectivity = new_areas, connectivity_baseline = baseline_areas,
        interpatch_distance = 10, res = pc_res(lizard_areas_connected), species = "Blue-tongued Lizard")
    Output
      # A tibble: 3 x 7
        scenario   interpatch_distance res   species       n_patches effective_mesh_ha
        <chr>                    <dbl> <chr> <chr>             <int>             <dbl>
      1 baseline                    10 2x2   Blue-tongued~        73              4.47
      2 new                         10 2x2   Blue-tongued~        72              2.86
      3 difference                  10 2x2   Blue-tongued~        -1             -1.61
      # i 1 more variable: prob_connectedness <dbl>

# compare_connectivity() identifies changes in baseline/scenario

    Code
      results_compare
    Output
      # A tibble: 3 x 6
        scenario interpatch_distance species           n_patches effective_mesh_ha
        <chr>                  <dbl> <chr>                 <int>             <dbl>
      1 baseline                 200 Superb Fairy Wren       282           334.   
      2 new                      200 Superb Fairy Wren       283           333.   
      3 change                   200 Superb Fairy Wren         1            -0.965
      # i 1 more variable: prob_connectedness <dbl>

