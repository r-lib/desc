# we can add an author with ORCID via comment

    Code
      format(desc$get_authors()[2])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (<https://orcid.org/0000-0001-7098-9676>, he did it)"

# we can add an author with ORCID

    Code
      format(desc$get_authors()[2])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (he did it, <https://orcid.org/0000-0001-7098-9676>)"

# we can add an ORCID to an author

    Code
      format(desc$get_authors()[5])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (Really?, <https://orcid.org/0000-0001-7098-9676>)"

# we can replace the ORCID of an author

    Code
      format(desc$get_authors()[1])
    Output
      [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph] (<https://orcid.org/1000-0003-4757-117X>)"

# add_me can use ORCID_ID

    Code
      format(desc$get_authors()[5])
    Output
      [1] "Bugs Bunny <bugs.bunny@acme.com> [ctb] (<https://orcid.org/0000-0002-0775-162X>)"

