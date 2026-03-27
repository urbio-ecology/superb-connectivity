# sf_fragment_habitat returns an sf object with id column

    Code
      names(result)
    Output
      [1] "id" "fg"

# sf_assign_patches_to_fragments assigns patch_id column

    Code
      names(result)
    Output
      [1] "geometry" "patch_id"

# sf_assign_patches_to_fragments assigns connected patches same id

    Code
      unique(result$patch_id)
    Output
      [1] 1

