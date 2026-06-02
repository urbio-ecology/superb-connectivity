# multiplication works

    Code
      compare_connectivity(new_areas, baseline_areas, distance = 10, species = "Blue-tongued Lizard")
    Output
      # A tibble: 3 x 6
        scenario   distance species     n_patches effective_mesh_ha prob_connectedness
        <chr>         <dbl> <chr>           <int>             <dbl>              <dbl>
      1 baseline         10 Blue-tongu~        59              4.49         0.0000171 
      2 new              10 Blue-tongu~        58              2.87         0.0000109 
      3 difference       10 Blue-tongu~        -1             -1.62        -0.00000618

