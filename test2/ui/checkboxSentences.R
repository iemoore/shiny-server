



checkboxGroupInput("checkGroup", label = h3("Columns"), 
                   choices = list("Verb" = 1, 
                                  "Spanish" = 2, 
                                  "English" = 3,
                                  "Tense" = 4,
                                  "Like/Dislike" = 5,
                                  "Save" = 6
                                  ),
                   selected = c(1:4))  