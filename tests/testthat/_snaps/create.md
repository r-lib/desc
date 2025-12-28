# partially named vectors passed to 'text' fail

    Code
      description$new(text = c(foo = "bar", "baz"))
    Condition
      Error in `idesc_create_text()`:
      ! text arg cannot have a mix of named and unnamed elements

# From installed package

    Code
      description$new(package = "fgsdgsdhldsknfglkedsfgsdf")
    Condition
      Error in `idesc_create_package()`:
      ! Cannot find DESCRIPTION for installed package fgsdgsdhldsknfglkedsfgsdf

