# summarise_connectivity returns a tibble with expected columns

    Code
      names(result)
    Output
       [1] "species"             "interpatch_distance" "n_patches"          
       [4] "effective_mesh_ha"   "prob_connectedness"  "patch_area_mean"    
       [7] "patch_area_total_ha" "target_resolution"   "data_resolution"    
      [10] "aggregation_factor" 

---

    Code
      result
    Output
      # A tibble: 1 x 10
        species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
        <chr>                     <dbl>     <int>             <dbl>              <dbl>
      1 Test Speci~                 100         1                 1             0.0001
      # i 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
      #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>

