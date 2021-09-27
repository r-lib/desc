# str formats authors properly

    Code
      cat(crayon::strip_style(desc$str(by_field = TRUE)[["Authors@R"]]))
    Output
      Authors@R:c(
          person("Hadley", "Wickham", , "h.wickham@gmail.com", role = c("aut", "cre", "cph")),
          person("Peter", "Danenberg", , "pcd@roxygen.org", role = c("aut", "cph")),
          person("Manuel", "Eugster", role = c("aut", "cph")),
          person("RStudio", role = "cph")
        )

