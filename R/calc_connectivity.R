# the connectivity calculation
calc_connectivity <- function(groupedAreas) {
  effMesh <- sum(groupedAreas$areaSquared) / sum(groupedAreas$total_area)
  # convert to hectares
  effMesh <- effMesh * 0.0001
  # return
  effMesh
}
