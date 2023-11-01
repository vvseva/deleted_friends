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
          textAreaInput(
            inputId = str_glue("textAreaInput_reason1_r_{id}"),
            label = str_glue("In a few sentences, try to describe why did you unfriend {f_name} on Facebook?")
          ),
          h4("Political Ideology"),
          radioButtons(
            inputId = str_glue("radioButtons_political1_r_{id}"),
            label = str_glue("Please indicate what you perceive {f_name} political ideology to be, to the best of your recollection."),
            choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Don’t know"),
            selected = character(0)
            ),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_political1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(
            inputId = str_glue("radioButtons_political2_r_{id}", id = id),
            label = str_glue("How often, if ever, did you see {f_name} share things about political or social issues on Facebook?"),
            choices = c(
              "Frequently", "Rarely", 
              "Never", "Unsure/don’t remember"),
            selected = character(0)
          ),
          h4("Religiosity"),
          radioButtons(
            inputId = str_glue("radioButtons_religion1_r_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} religious affiliation to be, to the best of your recollection. "),
            choices = c(
              "Christian", "Muslim", 
              "Jewish", "Hindu",
              "Buddhist", "Atheist/Agnostic",
              "Unsure",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_religion_r_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_religion1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(
            inputId = str_glue("radioButtons_religion2_r_{id}", id = id),
            label = str_glue("Please indicate the extent to which you encounter(ed) religious content from {f_name},  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          h4("Age"),
          numericInput(
            inputId = str_glue("numericInput_age1_r_{id}", id = id),
            label = "Please provide the approximate age that you believe the connection to be. This does not need to be exact",
            value = NULL,
            min = 16, 
            max = 100
          ),
          checkboxInput(
            inputId = str_glue("checkboxInput_age2_r_{id}", id = id),
            label = "Unsure"
          ),
          h4("Ethnic Status"),
          radioButtons(
            inputId = str_glue("radioButtons_ethinc1_r_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name}'s ethnicity to be, to the best of your recollection."),
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
          textInput(inputId = str_glue("textInput_ethinc_r_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_ethinc1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Gender Identity"),
          radioButtons(
            inputId = str_glue("radioButtons_gener_r_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} gender to be, to the best of your recollection."),
            choices = c(
              "Male", "Female", 
              "Non-binary/Third gender", "Unsure",
              "Prefer to self-describe"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_gender_r_{id}", id = id),
                    label = "Prefer to self-describe"),
          sliderTextInput(
            inputId = str_glue("sliderTextInput_gender1_r_{id}"),
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
          h4("Education Status"),
          radioButtons(
            inputId = str_glue("radioButtons_education_r_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} highest level of education to be, to the best of your recollection."),
            choices = c(
              "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
              "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
              "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)",
              "Unsure"),
            selected = character(0)
          ),
          h4("Online/Offline"),
          radioButtons(
            inputId = str_glue("radioButtons_onlineoffline_r_{id}", id = id),
            label = str_glue("Please indicate how you interacted with the {f_name}."),
            choices = c(
              "Online exclusively", "Primarily online", 
              "Primarily offline", "Both online and offline",
              "Unsure"),
            selected = character(0)
          ),
          h4("Meeting Opportunities"),
          radioButtons(
            inputId = str_glue("radioButtons_meeting_r_{id}", id = id),
            label = str_glue("At the time that you unfriended {f_name}, when was the last time you had encountered them in-person/virtual?"),
            choices = c(
              "Within a week prior to unfriending", 
              "Within a month prior to unfriending", 
              "Within a year prior to unfriending", 
              "More than a year prior",
              "Never",
              "Unsure"),
            selected = character(0)
          ),
          h4("Context"),
          checkboxGroupInput(
            inputId = str_glue("checkboxGroupInput_context_r_{id}", id = id),
            label = str_glue("Please indicate the context that you have interacted with {f_name}, to the best of your recollection. Select all that apply"),
            choices = c(
              "Residential (around the house)", "Recreational (in leisure)", 
              "Vocational (at work)", "Educational (at school)",
              "Religious (at events)", "Military", 
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_context_r_{id}", id = id),
                    label = "Other (please specify)"),
          
          h4("Social Relationships"),
          checkboxGroupInput(
            inputId = str_glue("checkboxGroupInput_relationship_r_{id}", id = id),
            label = str_glue("Please describe the social relationship(s) that you have shared with {f_name}, to the best of your recollection (e.g., friend, coworker, etc.)."),
            choices = c(
              "Acquaintance", "Neighbor", 
              "Friend", "Coworker or colleague",
              "Teacher/student", "Family by blood",
              "Family by marriage", "Romantic or sexual partner",
              "None", "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_relationship_r_{id}", id = id),
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
          h4("Political Ideology"),
          radioButtons(
            inputId = str_glue("radioButtons_political1_e_{id}"),
            label = str_glue("Please indicate what you perceive {f_name} political ideology to be, to the best of your recollection."),
            choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Unsure"),
            selected = character(0)
            ),
          radioButtons(
            inputId = str_glue("radioButtons_political2_e_{id}", id = id),
            label = str_glue("How often, if ever, did you see {f_name} share things about political or social issues on Facebook?"),
            choices = c(
              "Frequently", "Rarely", 
              "Never", "Unsure/don’t remember"),
            selected = character(0)
          ),
          h4("Religiosity"),
          radioButtons(
            inputId = str_glue("radioButtons_religion1_e_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} religious affiliation to be, to the best of your recollection. "),
            choices = c(
              "Christian", "Muslim", 
              "Jewish", "Hindu",
              "Buddhist", "Atheist/Agnostic",
              "Unsure",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_religion_e_{id}", id = id),
                    label = "Other (please specify)"),
          radioButtons(
            inputId = str_glue("radioButtons_religion2_e_{id}", id = id),
            label = str_glue("Please indicate the extent to which you encounter(ed) religious content from {f_name},  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          h4("Ethnic Status"),
          radioButtons(
            inputId = str_glue("radioButtons_ethinc1_e_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name}'s ethnicity to be, to the best of your recollection."),
            choices = c(
              "White or Caucasian", "Black or African American", 
              "Hispanic or Latino", "Asian",
              "Native American", "Native Hawaiian or Other Pacific Islander",
              "Middle Eastern or Arab", "Multiracial/Mixed",
              "Unsure",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_ethinc_e_{id}", id = id),
                    label = "Other (please specify)"),
          h4("Gender Identity"),
          radioButtons(
            inputId = str_glue("radioButtons_gener_e_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} gender to be, to the best of your recollection."),
            choices = c(
              "Male", "Female", 
              "Non-binary/Third gender", "Unsure",
              "Prefer to self-describe"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_gender_e_{id}", id = id),
                    label = "Prefer to self-describe"),
          h4("Sexual Orientation"),
          radioButtons(
            inputId = str_glue("radioButtons_orientation_e_{id}", id = id),
            label = str_glue("Please indicate what you perceive {f_name} sexual orientation to be, to the best of your recollection."),
            choices = c(
              "Heterosexual or Straight", "Unsure", 
              "Prefer to self-describe"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_orientation_e_{id}", id = id),
                    label = "Prefer to self-describe"),
          h4("Context"),
          checkboxGroupInput(
            inputId = str_glue("checkboxGroupInput_context_e_{id}", id = id),
            label = str_glue("Please indicate the context that you have interacted with {f_name}, to the best of your recollection."),
            choices = c(
              "Residential", "Vocational", 
              "Educational", "Religious",
              "Recreational", "Military",
              "None", "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_context_e_{id}", id = id),
                    label = "Other (please specify)"),
          
          h4("Social Relationships"),
          checkboxGroupInput(
            inputId = str_glue("checkboxGroupInput_relationship_e_{id}", id = id),
            label = str_glue("Please describe the social relationship(s) that you have shared with {f_name}, to the best of your recollection (e.g., friend, coworker, etc.)."),
            choices = c(
              "Acquaintance", "Neighbor", 
              "Friend", "Coworker or colleague",
              "Teacher/student", "Family by blood",
              "Family by marriage", "Romantic or sexual partner",
              "None", "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(inputId = str_glue("textInput_relationship_e_{id}", id = id),
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
        
        h4("Personal Values"),
        p("Now, please indicate your personal identifications with the following."),
        # h4("Political Ideology"),
        radioButtons(
          inputId = "radioButtons_political_p",
          label = "Political Ideology:",
          choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Unsure"),
          selected = character(0)
        ),
        radioButtons(
          inputId = "radioButtons_religion_p",
          label = "Religion:",
          choices = c(
            "Christian", "Muslim", 
            "Jewish", "Hindu",
            "Buddhist", "Atheist/Agnostic",
            "Unsure",
            "Other (please specify)"),
          selected = character(0)
        ),
        textInput(inputId = "textInput_religion_p",
                  label = "Other (please specify)"),
        radioButtons(
          inputId = "radioButtons_usage_p",
          label = "How frequently do you use Facebook?",
          choices = c("Multiple times a day", "Once a day",
                      "Once a week", "Rarely or never"),
          selected = character(0)
        ),
        h4("Demographic Questions"),
        numericInput(
          inputId = "numericInput_age_p",
          label = "How old are you?",
          min = 16,
          max = 100,
          value = NULL
        ),
        radioButtons(
          inputId = "radioButtons_gener_p",
          label = "Gender",
          choices = c(
            "Male", "Female", 
            "Non-binary/Third gender", "Unsure",
            "Prefer to self-describe"),
          selected = character(0)
        ),
        ## TODO: add conditional text for other
        textInput(inputId = "textInput_gender_p",
                  label = "Prefer to self-describe"),
        textInput(inputId = "textInput_ethnicity_p",
                  label = "Ethnicity"),
        radioButtons(
          inputId = "radioButtons_education_p",
          label = "Educational Level:",
          choices = c(
            "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
            "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
            "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)",
            "Unsure"),
          selected = character(0)
        ),
        radioButtons(
          inputId = "radioButtons_employment_p",
          label = "Employment Status:",
          choices = c(
            "Employed full-time", "Employed part-time", 
            "Unemployed", "Student (full-time)",
            "Student (part-time)", "Doctorate or Professional Degree (e.g., PhD, MD)",
            "Unsure"),
          selected = character(0)
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
            "Prefer not to say"),
          selected = character(0)
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
            "Prefer not to say"),
          selected = character(0)
        ),
        
        
    )
  )
}


track_that <- function(mongo_batch_track, 
                       time,
                       session,
                       stage = "test"){
  mongo_batch_track$insert(
    data.frame(
      session = session,
      time = time,
      stage = stage
    )
  )
}
