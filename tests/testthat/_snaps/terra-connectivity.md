# add_patch_area returns a two-layer raster named patch_id and area

    Code
      names(result)
    Output
      [1] "patch_id" "area"    

# aggregate_connected_patches returns tibble with correct columns

    Code
      names(res_con_patch)
    Output
      [1] "patch_id"     "area"         "area_squared"

# habitat_connectivity returns a data frame with expected columns

    Code
      names(result$result)
    Output
      [1] "patch_id"     "area"         "area_squared"

# habitat_connectivity_full returns list with expected elements

    Code
      names(result)
    Output
      [1] "buffered_habitat"  "patch_id_raster"   "areas_connected"  
      [4] "barrier_mask"      "remaining_habitat"

