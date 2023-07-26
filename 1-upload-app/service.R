render_question <- function(deleted_friend){
  f_name <- deleted_friend$name
  id <- deleted_friend$id
  timestamp <- deleted_friend$timestamp
  
  if (is_empty(deleted_friend)) {
    column(
      width = 8,
      div(class = "questions_div",
          p("Empty friend list")
          )
    )
  } else {
    column(
      width = 8,
      div(class = "questions_div",
          h4(str_glue("Who is {f_name}, that you deleted at {timestamp}")),
          sliderInput(
            # class = "slider_likert",
            label = "How Much Is the Fish?",
            min = 0, max = 100, value = 50,
            inputId = str_glue("slider_likert_{id}", id = id)
            ),
          textAreaInput(
            # class = "areainput",
            label = str_glue("Explain in few sentances why {f_name} ?"),
            inputId = str_glue("areainput_{id}", id = id)
          )
      )
    )
  }
}