purple_colfunc <- colorRampPalette(c("#E4E0EE", "#8370AA"))

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
      width = 12,
      div(class = str_glue("questions_div_{id}"),
          style = str_glue("box-sizing: border-box;
                            margin: 25px;
                           background-color:{bg_color}; 
                           border-radius: 25px;
                           background:linear-gradient(white, {bg_color});"),
          h4(str_glue("We will ask you a set of question about {f_name}, who you deleted on {timestamp}")),
          textAreaInput(
            width = "100%",
            inputId = str_glue("textAreaInput_reason1_r_{id}"),
            label = str_glue("In a few sentences, please describe why you unfriended {f_name} on Facebook?")
          ),
          h4("Political Ideology"),
          radioButtons(
            width = "100%",
            inputId = str_glue("radioButtons_political1_r_{id}"),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s political ideology."),
            choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Don’t know"),
            selected = character(0)
            ),
          textInput(width = "100%",inputId = str_glue("textInput_political1_r_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_political1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_political2_r_{id}", id = id),
            label = str_glue("How often, if ever, did you encounter {f_name} share or post content about political or social issues on Facebook?"),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_political2_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Religiosity"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_religion1_r_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s religious affiliation."),
            choices = c(
              "Christian", "Muslim", 
              "Jewish", "Hindu",
              "Buddhist", "Atheist/Agnostic",
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_religion_r_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_religion1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_religion2_r_{id}", id = id),
            label = str_glue("How often, if ever, did you encounter {f_name} share or post content about religion,  to the best of your recollection."),
            choices = c(
              "Frequently", "Rarely", 
              "Never"),
            selected = character(0)
          ),
          # h4("Age"),
          # numericInput(
          #   inputId = str_glue("numericInput_age1_r_{id}", id = id),
          #   label = "Please provide the approximate age (in years) of {f_name}. This does not need to be exact",
          #   value = NULL,
          #   min = 12, 
          #   max = 100
          # ),
          # sliderTextInput(width = "100%",
          #   inputId = str_glue("sliderTextInput_age1_r_{id}"),
          #   label = "How confident are you in this assessment?",
          #   grid = TRUE,
          #   force_edges = TRUE,
          #   choices = c("Not at all confident", 
          #               "Not confident", "Somewhat confident", 
          #               "Confident", "Very Confident")
          # ),
          h4("Ethnic Status"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_ethinc1_r_{id}", id = id),
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
          textInput(width = "100%",inputId = str_glue("textInput_ethinc_r_{id}", id = id),
                    label = "Other (please specify)"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_ethinc1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Gender Identity"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_gener_r_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s gender."),
            choices = c(
              "Male", "Female", 
              "Non-binary/Third gender",
              "Prefer to describe"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_gender_r_{id}", id = id),
                    label = "Prefer to describe"),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_gender1_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          # h4("Sexual Orientation"),
          # radioButtons(width = "100%",
          #   inputId = str_glue("radioButtons_orientation_{id}", id = id),
          #   label = str_glue("Please indicate what you perceive {f_name} sexual orientation to be, to the best of your recollection."),
          #   choices = c(
          #     "Heterosexual or Straight", "Unsure", 
          #     "Prefer to self-describe")
          # ),
          # ## TODO: add conditional text for other
          # textInput(width = "100%",inputId = str_glue("textInput_orientation_{id}", id = id),
          #           label = "Prefer to self-describe"),
          # h4("Education Status"),
          # radioButtons(width = "100%",
          #   inputId = str_glue("radioButtons_education_r_{id}", id = id),
          #   label = str_glue("To the best of your recollection, please indicate what you perceived as {f_name}'s highest level of education."),
          #   choices = c(
          #     "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
          #     "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
          #     "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)"),
          #   selected = character(0)
          # ),
          # sliderTextInput(width = "100%",
          #   inputId = str_glue("sliderTextInput_education_r_{id}"),
          #   label = "How confident are you in this assessment?",
          #   grid = TRUE,
          #   force_edges = TRUE,
          #   choices = c("Not at all confident", 
          #               "Not confident", "Somewhat confident", 
          #               "Confident", "Very Confident")
          # ),
          h4("Online/Offline"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_onlineoffline_r_{id}", id = id),
            label = str_glue("Please indicate how you interacted with {f_name}."),
            choices = c(
              "Online exclusively", "Primarily online", 
              "Primarily offline", "Both online and offline", "Never"),
            selected = character(0)
          ),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_onlineoffline_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Meeting Opportunities"),
          radioButtons(width = "100%",
            inputId = str_glue("radioButtons_meeting_r_{id}", id = id),
            label = str_glue("At the time that you unfriended {f_name}, when was the last time you had encountered them in-person/virtual?"),
            choices = c(
              "Within a week prior to unfriending", 
              "Within a month prior to unfriending", 
              "Within a year prior to unfriending", 
              "More than a year prior",
              "Never"),
            selected = character(0)
          ),
          sliderTextInput(width = "100%",
            inputId = str_glue("sliderTextInput_meeting_r_{id}"),
            label = "How confident are you in this assessment?",
            grid = TRUE,
            force_edges = TRUE,
            choices = c("Not at all confident", 
                        "Not confident", "Somewhat confident", 
                        "Confident", "Very Confident")
          ),
          h4("Context"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_context_r_{id}", id = id),
            label = str_glue("To the best of your recollection, please indicate the context in which you have interacted with {f_name}. Select all that apply"),
            choices = c(
              "Residential (around the house)", "Recreational (in leisure)", 
              "Vocational (at work)", "Educational (at school)",
              "Religious (at events)", "Military", 
              "Other (please specify)"),
            selected = character(0)
          ),
          ## TODO: add conditional text for other
          textInput(width = "100%",inputId = str_glue("textInput_context_r_{id}", id = id),
                    label = "Other (please specify)"),
          
          h4("Social Relationships"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_relationship_r_{id}", id = id),
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
          textInput(width = "100%",inputId = str_glue("textInput_relationship_r_{id}", id = id),
                    label = "Other (please specify)"),
          checkboxGroupInput(width = "100%",
            inputId = str_glue("checkboxGroupInput_nconnection_r_{id}", id = id),
            label = str_glue("How many mutual friends or connections did {f_name} have within your social network? Please provide your best estimate."),
            selected = c(0),
            choices = c("None", "a few (1-5)", "some (6-25)", "many (over 25)", "I honestly do not know")
          )
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
        p("Finally, please answer a set of questions about yourself"),
        # h4("Political Ideology"),
        radioButtons(width = "100%",
          inputId = "radioButtons_political_p",
          label = "Political Ideology:",
          choices = c(
            "Very Liberal", "Liberal", 
            "Moderate", "Conservative", 
            "Very Conservative", "Prefer not to say"),
          selected = character(0)
        ),
        textInput(width = "100%",inputId = "textInput_political_p",
                  label = "Other (please specify)"),
        radioButtons(width = "100%",
          inputId = "radioButtons_religion_p",
          label = "Religion:",
          choices = c(
            "Christian", "Muslim", 
            "Jewish", "Hindu",
            "Buddhist", "Atheist/Agnostic",
            "Prefer not to say",
            "Other (please specify)"),
          selected = character(0)
        ),
        textInput(width = "100%",inputId = "textInput_religion_p",
                  label = "Other (please specify)"),
        radioButtons(width = "100%",
          inputId = "radioButtons_usage_p",
          label = "How frequently do you use Facebook?",
          choices = c("Multiple times a day", "Once a day",
                      "Once a week", "Rarely or never"),
          selected = character(0)
        ),
        h4("Demographic Questions"),
        numericInput(
          inputId = "numericInput_age_p",
          label = "How old are you (in years)?",
          min = 18,
          max = 100,
          value = NULL
        ),
        radioButtons(width = "100%",
          inputId = "radioButtons_gener_p",
          label = "Gender",
          choices = c(
            "Male", "Female", 
            "Non-binary/Third gender",
            "Prefer to self-describe"),
          selected = character(0)
        ),
        ## TODO: add conditional text for other
        textInput(width = "100%",inputId = "textInput_gender_p",
                  label = "Prefer to self-describe"),
        radioButtons(width = "100%",
          inputId = str_glue("radioButtons_ethinc1_p"),
          label = str_glue("Please indicate what your ethnicity."),
          choices = c(
            "White or Caucasian", "Black or African American", 
            "Hispanic or Latino", "Asian",
            "Native American", "Native Hawaiian or Other Pacific Islander",
            "Middle Eastern or Arab", "Multiracial/Mixed",
            "Other (please specify)"),
          selected = character(0)
        ),
        ## TODO: add conditional text for other
        textInput(width = "100%",inputId = str_glue("textInput_ethinc_p"),
                  label = "Other (please specify)"),
        radioButtons(width = "100%",
          inputId = "radioButtons_education_p",
          label = "Educational Level:",
          choices = c(
            "Less than High School", "High School Diploma or Equivalent (e.g., GED)", 
            "Associate's Degree (e.g., AA, AS)", "Bachelor's Degree (e.g., BA, BS)",
            "Master's Degree (e.g., MA, MS)", "Doctorate or Professional Degree (e.g., PhD, MD)",
            "Prefer not to say"),
          selected = character(0)
        ),
        # radioButtons(width = "100%",
        #   inputId = "radioButtons_employment_p",
        #   label = "Employment Status:",
        #   choices = c(
        #     "Employed full-time", "Employed part-time", 
        #     "Unemployed", "Student (full-time)",
        #     "Student (part-time)", "Doctorate or Professional Degree (e.g., PhD, MD)",
        #     "Prefer not to say"),
        #   selected = character(0)
        # ),
        radioButtons(width = "100%",
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
        radioButtons(width = "100%",
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
