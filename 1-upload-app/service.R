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
            "Very Conservative", "Don’t know")
            ),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_political1_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
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
          sliderTextInput(
            inputId = str_glue("sliderTextInput_religion1_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(
            inputId = str_glue("radioButtons_religion2_{id}", id = id),
            label = str_glue("Please indicate the extent to which you encounter(ed) religious content from {f_name},  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never")
          ),
          h5("Age"),
          numericInput(
            inputId = str_glue("numericInput_age1_{id}", id = id),
            label = "Please provide the approximate age that you believe the connection to be. This does not need to be exact.",
            value = 0,
            min = 0, 
            max = 100
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
              "Don’t know",
              "Other (please specify)")
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_ethinc_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_ethinc1_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
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
          sliderTextInput(
            inputId = str_glue("sliderTextInput_gender1_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          # h5("Sexual Orientation"),
          # radioButtons(
          #   inputId = str_glue("radioButtons_orientation_{id}", id = id),
          #   label = str_glue("Please indicate what you perceive {f_name} sexual orientation to be, to the best of your recollection."),
          #   choices = c(
          #     "Heterosexual or Straight", "Unsure", 
          #     "Preffer to self-describe")
          # ),
          # ## TODO: add conditional text for other
          # textInput(inputId = str_glue("textInput_orientation_{id}", id = id),
          #           label = "Preffer to self-describe"),
          h5("Education Status"),
          radioButtons(
            inputId = str_glue("radioButtons_education_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} highest level of education to be, to the best of your recollection."),
            choices = c(
              "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
              "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
              "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)",
              "Unsure")
          ),
          h5("Online/Offline"),
          radioButtons(
            inputId = str_glue("radioButtons_onlineoffline_{id}", id = id),
            label = str_glue("Please indicate how you interacted with the {f_name}."),
            choices = c(
              "Online exclusively", "Primarily online", 
              "Primarily offline", "Both online and offline",
              "Unsure")
          ),
          h5("Meeting Opportunities"),
          radioButtons(
            inputId = str_glue("radioButtons_meeting_{id}", id = id),
            label = str_glue("At the time that you unfriended {f_name}, when was the last time you had encountered them in-person/virtual?"),
            choices = c(
              "Within a week prior to unfriending", 
              "Within a month prior to unfriending", 
              "Within a year prior to unfriending", 
              "More than a year prior",
              "Unsure")
          ),
          h5("Context"),
          radioButtons(
            inputId = str_glue("radioButtons_context_{id}", id = id),
            label = str_glue("Please indicate the context that you have interacted with {f_name}, to the best of your recollection."),
            choices = c(
              "Residential (around the house)", "Recreational (in leisure)", 
              "Vocational (at work)", "Educational (at school)",
              "Religious (at church)", "Military", 
              "Other (please specify)")
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
      # width = 6,
      div(class = str_glue("questions_div_{id}"),
          style = str_glue("box-sizing: border-box;
                            margin: 25px;
                           background-color:{bg_color}; 
                           border-radius: 25px;
                           background:linear-gradient(white, {bg_color});"),
          h4(str_glue("Who is {f_name}, that you added at {timestamp}")),
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

render_question_p <- function(){
  fluidRow(
    # width = 6,
    div(class = str_glue("questions_personal_div"),
        style = str_glue("box-sizing: border-box;
                            margin: 25px;
                           background-color:#32a852; 
                           border-radius: 25px;
                           background:linear-gradient(white, #32a852);"),
        
        h5("Personal Values"),
        p("Now, please indicate your personal identifications with the following."),
        # h5("Political Ideology"),
        radioButtons(
          inputId = "radioButtons_political_p",
          label = "Political Ideology:",
          choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Unsure")
        ),
        radioButtons(
          inputId = "radioButtons_religion_p",
          label = "Religion:",
          choices = c(
            "Christian", "Muslim", 
            "Jewish", "Hindu",
            "Buddhist", "Atheist/Agnostic",
            "Unsure",
            "Other (please specify)")
        ),
        textInput(inputId = "textInput_religion_p",
                  label = "Other (please specify)"),
        radioButtons(
          inputId = "radioButtons_usage_p",
          label = "How frequently do you use Facebook?",
          choices = c("Multiple times a day", "Once a day",
                      "Once a week", "Rarely or never")
        ),
        h5("Demographic Questions"),
        numericInput(
          inputId = "numericInput_age_p",
          label = "How old are you?",
          min = 0,
          max = 100,
          value = 0
        ),
        radioButtons(
          inputId = "radioButtons_gener_p",
          label = "Gender",
          choices = c(
            "Male", "Female", 
            "Non-binary/Third gender", "Unsure",
            "Preffer to self-describe")
        ),
        ## TODO: add conditional text for other
        textInput(inputId = "textInput_gender_p",
                  label = "Preffer to self-describe"),
        textInput(inputId = "textInput_ethnicity_p",
                  label = "Ethnicity"),
        radioButtons(
          inputId = "radioButtons_education_p",
          label = "Educational Level:",
          choices = c(
            "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
            "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
            "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)",
            "Unsure")
        ),
        radioButtons(
          inputId = "radioButtons_employment_p",
          label = "Employment Status:",
          choices = c(
            "Employed full-time", "Employed part-time", 
            "Unemployed", "Student (full-time)",
            "Student (part-time)", "Doctorate or Professional Degree (e.g., PhD, MD)",
            "Unsure")
        ),
        radioButtons(
          inputId = "radioButtons_income_p",
          label = "Annual Household Income:",
          choices = c(
            "Less than $20,000", 
            "$20,000 - $39,999",
            "$40,000 - $59,999",
            "$60,000 - $79,999",
            "$80,000 - $99,999",
            "$100,000 - $149,999",
            "$150,000 or more",
            "Prefer not to say")
        ),
        radioButtons(
          inputId = "radioButtons_marital_p",
          label = "Relationship Status:",
          choices = c(
            "Single", 
            "In a relationship",
            "Married",
            "Divorced",
            "Widowed",
            "Prefer not to say")
        ),
        
        
    )
  )
}
