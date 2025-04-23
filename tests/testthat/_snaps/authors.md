# we can add an author with ORCID via comment

    Code
      format(desc$get_authors()[2])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (ORCID: <https://orcid.org/0000-0001-7098-9676>, what: he did it)"

# we can add an author with ORCID

    Code
      format(desc$get_authors()[2])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (what: he did it, ORCID: <https://orcid.org/0000-0001-7098-9676>)"

# we cannot add an author with malformatted comment

    Code
      desc$add_author("Gábor", "Csárdi", email = "csardi.gabor@gmail.com", role = "ctb",
        comment = c(ORCID = "orcid_number", what = NA))
    Condition
      Error in `check_author_args()`:
      ! is_named_character_or_null(comment) is not TRUE

# we can add an ORCID to an author

    Code
      format(desc$get_authors()[5])
    Output
      [1] "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (Really?, ORCID: <https://orcid.org/0000-0001-7098-9676>)"

# we can replace the ORCID of an author

    Code
      format(desc$get_authors()[1])
    Condition
      Warning in `person1()`:
      Invalid ORCID iD: '1000-0003-4757-117X'.
    Output
      [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph] (ORCID: <https://orcid.org/1000-0003-4757-117X>)"

# we cannot add the same ORCID to more than one author

    Code
      desc$add_orcid(given = "Peter", orcid = "orcidid")
    Condition
      Error:
      ! More than one author correspond to the provided arguments.
               ORCID IDs have to be distinct.

---

    Code
      desc$add_orcid(given = "Manuel", orcid = "0000-0003-4757-117X")
    Condition
      Error:
      ! Already an author with this ORCID ID.
               ORCID IDs have to be distinct.

# we cannot add the same ROR to more than one author

    Code
      desc$add_ror(given = "Peter", ror = "bla")
    Condition
      Error:
      ! More than one author correspond to the provided arguments.
               ROR IDs have to be distinct.

---

    Code
      desc$add_ror(given = "Manuel", ror = "012345678")
    Condition
      Error:
      ! Already an author with this ROR ID.
               ROR IDs have to be distinct.

# add_me can use ORCID_ID

    Code
      format(desc$get_authors()[5])
    Output
      [1] "Bugs Bunny <bugs.bunny@acme.com> [ctb] (ORCID: <https://orcid.org/0000-0002-0775-162X>)"

# error if not Authors@R field

    Code
      desc$get_authors()
    Condition
      Error in `ensure_authors_at_r()`:
      ! No 'Authors@R' field!
      You can create one with $add_author.
      You can also use $coerce_authors_at_r() to change Author fields

# coerce_authors_at_r if there is no Authors@R field

    Code
      D1$get_authors()
    Condition
      Error in `ensure_authors_at_r()`:
      ! No 'Authors@R' field!
      You can create one with $add_author.
      You can also use $coerce_authors_at_r() to change Author fields

# coerce_authors_at_r errors if no authors fields at all

    Code
      D1$coerce_authors_at_r()
    Condition
      Error in `idesc_coerce_authors_at_r()`:
      ! No 'Authors@R' or 'Author' field!
      You can create one with $add_author

# coerce_authors_at_r handles maintainer not being author

    Code
      D15$get_author("cre")
    Output
      [1] "Masami Saga <msaga@mtb.biglobe.ne.jp> [cre]"
    Code
      D15$get_author("aut")
    Output
      [1] "The Institute of Statistical Mathematics [aut]"

---

    Code
      D17$get_author("cre")
    Output
      [1] "Masami Saga <msaga@mtb.biglobe.ne.jp> [cre]"
    Code
      D17$get_author("aut")
    Output
      [1] "The Institute of Statistical Mathematics [aut]"
      [2] "another person <person@here.co> [aut]"         

# deparse_authors_at_r

    Code
      readLines(tmp)
    Output
      [1] "Authors@R: "                                                                              
      [2] "    person(\"Hadley\", \"Wickham\", , \"hadley@rstudio.com\", role = c(\"aut\", \"cre\"),"
      [3] "           comment = c(ORCID = \"0000-0003-4757-117X\"))"                                 

---

    Code
      readLines(tmp)
    Output
      [1] "Authors@R: c("                                                                            
      [2] "    person(\"Hadley\", \"Wickham\", , \"hadley@rstudio.com\", role = c(\"aut\", \"cre\"),"
      [3] "           comment = c(ORCID = \"0000-0003-4757-117X\")),"                                
      [4] "    person(\"Jennifer\", \"Bryan\", , \"jenny@rstudio.com\", role = \"aut\","             
      [5] "           comment = c(ORCID = \"0000-0002-6983-2759\")),"                                
      [6] "    person(\"RStudio\", role = c(\"cph\", \"fnd\"))"                                      
      [7] "  )"                                                                                      

