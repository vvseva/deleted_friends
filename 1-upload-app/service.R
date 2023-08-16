purple_colfunc <- colorRampPalette(c("#E4E0EE", "#836EAA"))

## TODO: wrap block into a function

render_question <- function(deleted_friend){
  
  f_name <- deleted_friend |> pull(name)
  id <- deleted_friend |> pull(id)
  timestamp <- deleted_friend |> pull(timestamp)
  bg_color <- deleted_friend |> pull(bg_color)
  
  if (is_empty(deleted_friend)) {
    fluidRow(
      # width = 12,
      div(class = "questions_div",
          p("Empty friend list")
          )
    )
  } else {
    fluidRow(
      # width = 6,
      div(class = str_glue("questions_div_{id}"),
          style = str_glue("box-sizing: border-box;
                            margin: 25px;
                           background-color:{bg_color}; 
                           border-radius: 25px;
                           background:linear-gradient(white, {bg_color});"),
          h4(str_glue("Who is {f_name}, that you deleted at {timestamp}")),
          h5("Political Ideology"),
          radioButtons(
            inputId = str_glue("radioButtons_political1_{id}"),
            label = str_glue("Please indicate what you perceive {f_name} political ideology to be, to the best of your recollection."),
            choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Unsure")
            ),
          radioButtons(
            inputId = str_glue("radioButtons_political2_{id}", id = id),
            label = str_glue("How often, if ever, did you see {f_name} share things about political or social issues on Facebook?"),
            choices = c(
              "Frequently", "Rarely", 
              "Never", "Unsure/don’t remember")
          ),
          h5("Religiosity"),
          radioButtons(
            inputId = str_glue("radioButtons_religion1_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} religious affiliation to be, to the best of your recollection. "),
            choices = c(
              "Christian", "Muslim", 
              "Jewish", "Hindu",
              "Buddhist", "Atheist/Agnostic",
              "Unsure",
              "Other (please specify)")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_religion_{id}", id = id),
                    label = "Other (please specify)"),
          radioButtons(
            inputId = str_glue("radioButtons_religion2_{id}", id = id),
            label = str_glue("Please indicate the extent to which you encounter(ed) religious content from {f_name},  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never")
          ),
          h5("Ethnic Status"),
          radioButtons(
            inputId = str_glue("radioButtons_ethinc1_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name}'s ethnicity to be, to the best of your recollection."),
            choices = c(
              "White or Caucasian", "Black or African American", 
              "Hispanic or Latino", "Asian",
              "Native American", "Native Hawaiian or Other Pacific Islander",
              "Middle Eastern or Arab", "Multiracial/Mixed",
              "Unsure",
              "Other (please specify)")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_ethinc_{id}", id = id),
                    label = "Other (please specify)"),
          h5("Gender Identity"),
          radioButtons(
            inputId = str_glue("radioButtons_gener_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} gender to be, to the best of your recollection."),
            choices = c(
              "Male", "Female", 
              "Non-binary/Third gender", "Unsure",
              "Preffer to self-describe")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_gender_{id}", id = id),
                    label = "Preffer to self-describe"),
          h5("Sexual Orientation"),
          radioButtons(
            inputId = str_glue("radioButtons_orientation_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} sexual orientation to be, to the best of your recollection."),
            choices = c(
              "Heterosexual or Straight", "Unsure", 
              "Preffer to self-describe")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_orientation_{id}", id = id),
                    label = "Preffer to self-describe"),
          h5("Context"),
          radioButtons(
            inputId = str_glue("radioButtons_context_{id}", id = id),
            label = str_glue("Please indicate the context that you have interacted with {f_name}, to the best of your recollection."),
            choices = c(
              "Residential", "Vocational", 
              "Educational", "Religious",
              "Recreational", "Military",
              "None", "Other (please specify)")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_context_{id}", id = id),
                    label = "Other (please specify)"),
          
          h5("Social Relationships"),
          radioButtons(
            inputId = str_glue("radioButtons_relationship_{id}", id = id),
            label = str_glue("Please describe the social relationship(s) that you have shared with {f_name}, to the best of your recollection (e.g., friend, coworker, etc.)."),
            choices = c(
              "Acquaintance", "Neighbor", 
              "Friend", "Coworker or colleague",
              "Teacher/student", "Family by blood",
              "Family by marriage", "Romantic or sexual partner",
              "None", "Other (please specify)")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_relationship_{id}", id = id),
                    label = "Other (please specify)"),
      ),

        
    )
  }
}
