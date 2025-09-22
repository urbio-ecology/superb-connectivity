# function to group the remaining habitat patches by area
group_connect_areas <- function(patchAreas) {
  grouped <- group_by(patchAreas, patchID)
  summed <- summarise(grouped, total_area = sum(area))
  # create a column with  area squared in it
  summed$areaSquared <- summed$total_area^2
  summed
}
