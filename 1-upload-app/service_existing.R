## TODO: wrap block into a function

render_question_2 <- function(deleted_friend){
  
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
      div(class = str_glue("questions_div_{id}"),
          style = str_glue("box-sizing: border-box;
                            margin: 25px;
                           background-color:{bg_color}; 
                           border-radius: 25px;
                           background:linear-gradient(white, {bg_color});"),
          h4(str_glue("We will ask you a set of question about {f_name}, who you added on {timestamp}")),
          textAreaInput(
            width = "100%",
            inputId = str_glue("textAreaInput_reason1_e_{id}"),
            label = str_glue("In a few sentences, please describe who is {f_name} to you?")
          ),
          h4("Political Ideology"),
          radioButtons(
            width = "100%",
            inputId = str_glue("radioButtons_political1_e_{id}"),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s political ideology."),
            choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Don’t know"),
            selected = character(0)
            ),
          textInput(width = "100%",
                    inputId = str_glue("textInput_political1_e_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_political1_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_political2_e_{id}", id = id),
            label = str_glue("How often, if ever, did you encounter {f_name} share or post content about political or social issues on Facebook?"),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_political2_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Religiosity"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_religion1_e_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s religious affiliation."),
            choices = c(
              "Christian", "Muslim", 
              "Jewish", "Hindu",
              "Buddhist", "Atheist/Agnostic",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_religion_e_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_religion1_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_religion2_e_{id}", id = id),
            label = str_glue("How often, if ever, did you encounter {f_name} share or post content about religion,  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          # h4("Age"),
          # numericInput(
          #   inputId = str_glue("numericInput_age1_e_{id}", id = id),
          #   label = str_glue("Please provide the approximate age (in years) of {f_name}. This does not need to be exact"),
          #   value = NULL,
          #   min = 12, 
          #   max = 100
          # ),
          # sliderTextInput(
          #   inputId = str_glue("sliderTextInput_age1_e_{id}"),
          #   label = "How confident are you in this assessment?",
          #   grid = TRUE,
          #   force_edges = TRUE,
          #   choices = c("Not at all confident", 
          #               "Not confident", "Somewhat confident", 
          #               "Confident", "Very Confident")
          # ),
          h4("Ethnic Status"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_ethinc1_e_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s ethnicity."),
            choices = c(
              "White or Caucasian", "Black or African American", 
              "Hispanic or Latino", "Asian",
              "Native American", "Native Hawaiian or Other Pacific Islander",
              "Middle Eastern or Arab", "Multiracial/Mixed",
              "Don’t know",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_ethinc_e_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_ethinc1_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Gender Identity"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_gener_e_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s gender."),
            choices = c(
              "Male", "Female", 
              "Non-binary/Third gender",
              "Prefer to describe"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_gender_e_{id}", id = id),
                    label = "Prefer to describe"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_gender1_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          # h4("Sexual Orientation"),
          # radioButtons(
          #   inputId = str_glue("radioButtons_orientation_{id}", id = id),
          #   label = str_glue("Please indicate what you perceive {f_name} sexual orientation to be, to the best of your recollection."),
          #   choices = c(
          #     "Heterosexual or Straight", "Unsure", 
          #     "Prefer to self-describe")
          # ),
          # ## TODO: add conditional text for other
          # textInput(inputId = str_glue("textInput_orientation_{id}", id = id),
          #           label = "Prefer to self-describe"),
          # h4("Education Status"),
          # radioButtons(
          #   inputId = str_glue("radioButtons_education_e_{id}", id = id),
          #   label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s highest level of education."),
          #   choices = c(
          #     "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
          #     "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
          #     "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)"),
          #   selected = character(0)
          # ),
          # sliderTextInput(
          #   inputId = str_glue("sliderTextInput_education_e_{id}"),
          #   label = "How confident are you in this assessment?",
          #   grid = TRUE,
          #   force_edges = TRUE,
          #   choices = c("Not at all confident", 
          #               "Not confident", "Somewhat confident", 
          #               "Confident", "Very Confident")
          # ),
          h4("Online/Offline"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_onlineoffline_e_{id}", id = id),
            label = str_glue("Please indicate how you interacted with {f_name}."),
            choices = c(
              "Online exclusively", "Primarily online", 
              "Primarily offline", "Both online and offline", "Never"),
            selected = character(0)
          ),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_onlineoffline_e_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Context"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_context_e_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate the context in which you have interacted with {f_name}. Select all that apply"),
            choices = c(
              "Residential (around the house)", "Recreational (in leisure)", 
              "Vocational (at work)", "Educational (at school)",
              "Religious (at events)", "Military", 
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_context_e_{id}", id = id),
                    label = "Other (please specify)"),
          
          h4("Social Relationships"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_relationship_e_{id}", id = id),
            label = str_glue("To the best of your recollection, please describe the social relationship(s) that you have shared with {f_name}, e.g., friend, coworker, etc.. Select all that apply"),
            choices = c(
              "Acquaintance", "Neighbor", 
              "Friend", "Coworker or colleague",
              "Teacher/student", "Family by blood",
              "Family by marriage", "Romantic or sexual partner",
              "None", "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_relationship_e_{id}", id = id),
                    label = "Other (please specify)"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_nconnection_e_{id}", id = id),
            label = str_glue("How many mutual friends or connections did {f_name} have within your social network? Please provide your best estimate."),
            selected = c(0),
            choices = c("None", "a few (1-5)", "some (6-25)", "many (over 25)", "I honestly do not know")
          )
      ),

        
    )
  }
}