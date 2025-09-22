# clean membership function to account for when some fragments appear in no buffered areas,
# or some appear in more than one.
clean_inter <- function(membership_vector) {
  # check for empty vectors
  if (length(membership_vector) == 0) {
    membership_vector <- NA
  }

  if (length(membership_vector) > 1) {
    membership_vector <- membership_vector[1]
    warning(
      "One of the fragments occurs in more than one buffered area, ",
      "so has been assigned to the first area in the list"
    )
  }
  membership_vector
}
