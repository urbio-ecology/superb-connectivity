# multiplication works

    Code
      compare_connectivity(connectivity = new_areas, connectivity_baseline = baseline_areas,
        interpatch_distance = 10, res = pc_res(lizard_areas_connected), species = "Blue-tongued Lizard")
    Output
      # A tibble: 3 x 7
        scenario   interpatch_distance res   species       n_patches effective_mesh_ha
        <chr>                    <dbl> <chr> <chr>             <int>             <dbl>
      1 baseline                    10 ""    Blue-tongued~        73              4.47
      2 new                         10 ""    Blue-tongued~        72              2.86
      3 difference                  10 ""    Blue-tongued~        -1             -1.61
      # i 1 more variable: prob_connectedness <dbl>

