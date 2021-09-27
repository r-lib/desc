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

