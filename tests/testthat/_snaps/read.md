# duplicate fields, #43

    Code
      description$new(test_path("D5"))
    Condition
      Error in `read_dcf()`:
      ! Duplicate DESCRIPTION fields: 'Remotes'

# empty lines error

    Code
      description$new(test_path("D12"))
    Condition
      Error:
      ! Empty lines found in DESCRIPTION file

