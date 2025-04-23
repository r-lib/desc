# get_description_from_package

    Code
      description$new(file.path("fixtures", "notpkg_1.0.tar.gz"))
    Condition
      Error in `get_description_from_package()`:
      ! Cannot extract DESCRIPTION from 'fixtures/notpkg_1.0.tar.gz'

# write errors if from archive

    Code
      d$write()
    Condition
      Error in `idesc_write()`:
      ! Cannot write back DESCRIPTION. Note that it is not possible
                to update DESCRIPTION files within package archives

