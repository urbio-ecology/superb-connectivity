# multiplication works

    Code
      compare_connectivity(new_areas, baseline_areas, interpatch_distance = 10,
        species = "Blue-tongued Lizard")
    Output
      # A tibble: 3 x 6
        scenario   interpatch_distance species             n_patches effective_mesh_ha
        <chr>                    <dbl> <chr>                   <int>             <dbl>
      1 baseline                    10 Blue-tongued Lizard        59              4.49
      2 new                         10 Blue-tongued Lizard        58              2.87
      3 difference                  10 Blue-tongued Lizard        -1             -1.62
      # i 1 more variable: prob_connectedness <dbl>

