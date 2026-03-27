# summarise_connectivity returns a tibble with expected columns

    Code
      names(result)
    Output
       [1] "species_name"        "buffer_distance"     "n_patches"          
       [4] "prob_connectedness"  "effective_mesh_ha"   "patch_area_mean"    
       [7] "patch_area_total_ha" "target_resolution"   "data_resolution"    
      [10] "aggregation_factor" 

---

    Code
      result
    Output
      # A tibble: 1 x 10
        species_name buffer_distance n_patches prob_connectedness effective_mesh_ha
        <chr>                  <dbl>     <int>              <dbl>             <dbl>
      1 Test Species             100         1             0.0001                 1
      # i 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
      #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>

