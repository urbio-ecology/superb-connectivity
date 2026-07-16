# patch_size class is compatible with dplyr

    Code
      filter(areas, area > 4000)
    Output
      # patch_size:          data.frame
      # Species:             Blue-tongued Lizard
      # Patches:             4
      # Resolution:          2x2
      # Interpatch Distance: 8 m
        patch_id   area
           <dbl>  <dbl>
      1       27 85541.
      2     1026  5508.
      3     1412 28878.
      4     1933 10169.

---

    Code
      filter(areas, area > 1000)
    Output
      # patch_size:          data.frame
      # Species:             Blue-tongued Lizard
      # Patches:             34
      # Resolution:          2x2
      # Interpatch Distance: 8 m
        patch_id   area
           <dbl>  <dbl>
      1        3  1304.
      2       27 85541.
      3       37  1876.
      4       52  1108.
      5      151  1964.
      # i 29 more rows

---

    Code
      slice(areas, 1:10)
    Output
      # patch_size:          data.frame
      # Species:             Blue-tongued Lizard
      # Patches:             10
      # Resolution:          2x2
      # Interpatch Distance: 8 m
        patch_id   area
           <dbl>  <dbl>
      1        1   60.0
      2        3 1304. 
      3       10   12.0
      4       11  892. 
      5       13   28.0
      # i 5 more rows

---

    Code
      head(select(areas, -area))
    Message
      Removing attributes in <patch_size>
    Output
        patch_id
      1        1
      2        3
      3       10
      4       11
      5       13
      6       14

---

    Code
      head(select(areas, -patch_id))
    Message
      Removing attributes in <patch_size>
    Output
            area
      1   60.004
      2 1304.087
      3   12.001
      4  892.060
      5   28.002
      6   40.003

