
library(shiny)
library(tidyverse)
library(jsonlite)
library(purrr)
library(colorspace)
library(shinyjs)
library(digest)
library(shinyWidgets)
library(rclipboard)

source("service.R")
source("service_existing.R")


## Storage
library(mongolite)

source("Secret.R")
user_mongo = Sys.getenv("user_mongo")
pass_mongo = Sys.getenv("pass_mongo")

connection_string = paste0('mongodb+srv://',
                           user_mongo, ':',
                           pass_mongo,
                           '@cluster0.9xhplvh.mongodb.net/?retryWrites=true&w=majority')

mongo_batch = mongo(db="fb_shiny", 
                    collection="test_0",
                    url=connection_string)

mongo_batch_track = mongo(db="fb_shiny", 
                    collection="tracking_0",
                    url=connection_string)

w = 9

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    # Application title
    titlePanel("Unfriending on Facebook"),

    # Sidebar with a slider input for number of bins 
        # sidebarPanel(
        #   div(id = "intro_div",
        #   HTML("
        #        <b> Instructions and opt-out: </b> <br>
        #        In the following survey, we will ask questions about 
        #        up to five of your most recently removed Facebook 
        #        connections, and up to five of your current connections. 
        #        If there are any individuals that you wish to avoid 
        #        discussing, please uncheck their names here.
        #        ")
        #   )
        # ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidPage(
            column(2),
            rclipboardSetup(),
            tags$script(
              "$(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'a') {
            $('#valueA').text(event.value);
          }
        });
        "
            ),
            # column(12,
          div(id = "upload_deleted_div",
              fileInput("upload_deleted", "Upload a facebook zip file",
                        accept = ".zip"),
              htmlOutput("html_confirm_upload")
              ),
          div(id = "questions_0_div",
              htmlOutput("html_confirm_friends")
          ),
          div(id = "questions_1_div",
              htmlOutput("html_deleted")
              ),
          div(id = "questions_2_div",
              htmlOutput("html_existing")
          ),
          div(id = "questions_p_div",
              htmlOutput("html_personal")
          ),
          div(id = "verification_div",
              htmlOutput("html_verification")
          ),
          div(id = "final_div",
              htmlOutput("html_final")
          )
        )
        # )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values <- reactiveValues(fb_df_removed = NULL,
                           fb_df_existing = NULL,
                           fb_df_filtered = NULL,
                           logs = NULL,
                           remove = FALSE)
  
  values$validation_code <- floor(runif(1, min=10000000, max=99999999))
  
  # values$fb_df <- tibble(name = NULL)

  observeEvent(input$upload_deleted, {
    
    
    fb_removed_df <- tryCatch({
      ### TODO: fix if not deleted friends
      
      unzip(zipfile = input$upload_deleted$datapath,
                     files  = "connections/friends/removed_friends.json",
                     exdir = tempdir()
      ) |> 
        read_json() |> 
        as_tibble()
      
    }, error=function(e) {
      # cat(paste("in err handler2\n"),e)
      data.frame(deleted_friends_v2 = NULL, timestamp = NULL)
    }, warning=function(w) {
      # cat(paste("in warn handler2\n"),w)
      data.frame(deleted_friends_v2 = NULL, timestamp = NULL)
    })

    if (nrow(fb_removed_df) < 1)  {
      output$html_confirm_upload <- renderUI(
          h3("Sorry, you have not deleted any friends in the past 3 years, and therefore you are not qualify for the next part of the study"),
          
          track_that(mongo_batch_track = mongo_batch_track, 
                     time = Sys.time(),
                     session = session$token,
                     stage = "html_confirm_friends_0")
      )
    } else {
      output$html_confirm_upload <- renderUI(
        column(
          width = w,
          p("You have successfully uploaded your file. 
           We will temporarily store it, but will not save it until the last step."),
        actionButton(inputId = "actionButton_fb_confirm_upload",
                     label = "Continue")
        )
      )
      
      values$fb_removed_df <- fb_removed_df
    }
  })
      
  
    observeEvent(input$actionButton_fb_confirm_upload, {
      hide("intro_div")
      hide("upload_deleted_div")
      
      fb_removed_df <- isolate(values$fb_removed_df) |>
        unnest_wider(col = deleted_friends_v2) |>
        mutate(timestamp = timestamp |>
                 as.Date.POSIXct())
      
    values$fb_df_removed <- fb_removed_df |>
      head(5) |>
      mutate(id = row_number()) |>
      mutate(bg_color = n() |>
               purple_colfunc() |>
               lighten(amount = 0.75)) |> 
      mutate(select_name = str_glue("{name} (removed at {date})", 
                                    name = name, date = timestamp))
    
    fb_your_df <- tryCatch({
      
      unzip(zipfile = input$upload_deleted$datapath,
            files  = "connections/friends/your_friends.json",
            exdir = tempdir()
      ) |> read_json() 
      
    }, error=function(e) {
      data.frame(friends_v2 = NULL, timestamp = NULL)
    }, warning=function(w) {
      data.frame(friends_v2 = NULL, timestamp = NULL)
    })
    
    values$fb_df_existing <- fb_your_df |>
      as_tibble() |>
      unnest_wider(col = friends_v2) |>
      mutate(timestamp = timestamp |>
               as.Date.POSIXct()) |>
      filter(!name %in% fb_removed_df$name) |> 
      sample_n(size = 5) |> ## TODO: replace with 5?
      mutate(id = row_number()) |>
      mutate(bg_color = n() |>
               purple_colfunc() |>
               lighten(amount = 0.75)) |> 
      mutate(select_name = str_glue("{name} (added at {date})", 
                                    name = name, date = timestamp))


    output$html_confirm_friends <- renderUI(
      column(
        width = w,
        checkboxGroupInput(
          inputId = "checkboxGroupInput_fb_confirm",
          label = "Please, uncheck names that you do not feel comfortable talking about.",
          choices = c(values$fb_df_existing$select_name, values$fb_df_removed$select_name),
          selected  = c(values$fb_df_existing$select_name, values$fb_df_removed$select_name)
        ),
        textAreaInput(
          inputId = "textAreaInput_fb_confirm_explanation",
          label = "If you have unchecked someone, please briefly explain why."
        ),
        actionButton(inputId = "actionButton_fb_confirm_selection",
                     label = "Confirm")
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_confirm_friends_1")
  })
  
  
  
  observeEvent(input$actionButton_fb_confirm_selection, {
    hide("questions_0_div")
    
    values$fb_df_removed_filtered <- isolate(values$fb_df_removed) |> 
      filter(select_name %in% input$checkboxGroupInput_fb_confirm)
    
    values$fb_df_existing_filtered <- isolate(values$fb_df_existing) |> 
      filter(select_name %in% input$checkboxGroupInput_fb_confirm)

    if(nrow(values$fb_df_removed_filtered) < 1){
      ## TODO: message or something
      output$html_deleted <- renderUI(
        h3("You removed all deleted friends. Unfortunately you could not continue the study. Please exit the survey.")
      )

    } else if (nrow(values$fb_df_existing_filtered) < 1) {
      output$html_deleted <- renderUI(
        h3("You removed all existing friends. Unfortunately you could not continue the study. Please exit the survey.")
      )
    } else {
      output$html_deleted <- renderUI(
        column(
          width = w,
        fluidRow(
        isolate(values$fb_df_removed_filtered) |>
          purrr::pmap(data.frame) |>
          purrr::map(render_question),
      ),
      fluidRow(
        div(
          actionButton(inputId = "actionButton1_existing",
                       label = "Next"),
          style = "display:inline-block; float:right;"
        ),
      )
      ))
    }
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_deleted")
    
  })
  
  observeEvent(input$actionButton1_existing, {
    hide("questions_1_div")
    
    
    values$fb_df_existing_filtered <- isolate(values$fb_df_existing) |> 
      filter(select_name %in% input$checkboxGroupInput_fb_confirm)
    
    if(nrow(values$fb_df_existing_filtered) < 1 ){
      ## TODO: message or something
      output$html_existing <- renderUI(
        h3("You removed all deleted friends. Unfortunately you could not continue the study. Please exit the survey.")
      )
      
    } else if (nrow(values$fb_df_removed_filtered) < 1) {
      output$html_existing <- renderUI(
        h3("You removed all existing friends. Unfortunately you could not continue the study. Please exit the survey.")
      )
    } else {
      output$html_existing <- renderUI(
        column(
          width = w,
          fluidRow(
            isolate(values$fb_df_existing_filtered) |>
              purrr::pmap(data.frame) |>
              purrr::map(render_question_2),
          ),
          fluidRow(
            div(
              actionButton(inputId = "actionButton1_removed",
                           label = "Next"),
              style = "display:inline-block; float:right;"
            ),
          )
        ))
    }
    
  })
  
  observeEvent(input$actionButton1_removed, {
    hide("questions_2_div")
    
    output$html_personal <- renderUI(
      column(
        width = w,
        fluidRow(
          render_question_p()
        ),
        fluidRow(
          div(
            actionButton(inputId = "actionButton1_verify",
                         label = "Next"),
            style = "display:inline-block; float:right;"
          ),
        )
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_personal")
  })
  
  observeEvent(input$actionButton1_verify, {
    hide("questions_p_div")
    
    
    # AllInputs <- reactive({
      x <- reactiveValuesToList(input)
      
      # print(x)
      # 
      # saveRDS(x, file = "x.rds")
      # 
      # x <- x |> 
      #   as.data.frame() |> 
      #   t() |> 
      #   data.frame() |> 
      #   rownames_to_column(var = "input") |> 
      #   pivot_longer(cols = -'input', names_to = "user") |> 
      #   mutate(
      #     session_user = session$user,
      #     time = Sys.time()
      #          )
    
      x <- x[-1] |> 
        unlist() |> 
        as.data.frame() |> 
        rename(value = "unlist(x[-1])") |> 
        rownames_to_column("input") |> 
        filter(!input |> str_detect("checkboxGroupInput_fb_confirm")) |> 
        bind_rows(
          isolate(values$fb_df_existing_filtered) |> 
            select(id, timestamp) |> 
            mutate(input = str_glue("existing_friend_id_{id}", id = id),
                   value = timestamp |> as.character()) |> 
            select(input, value)
        ) |> 
        bind_rows(
          isolate(values$fb_df_removed_filtered) |> 
            select(id, timestamp) |> 
            mutate(input = str_glue("removed_friend_id_{id}", id = id),
                   value = timestamp |> as.character()) |> 
            select(input, value)
        ) |> 
        mutate(
              session_user = session$token,
              timestamp = Sys.time()
                   )
      
       ## TODO: add time of friends
      if (values$remove == TRUE) {
        values$logs <- data.frame(
          input = "removed",
          session_user = session$token,
          time = Sys.time()
        )
      } else {
        values$logs <- x
        # x
      }
      

    # })
    
    
    output$html_verification <- renderUI(
      column(
        width = w,
        h3("After this step we will save your answers on the server. We will save only your answers, and will not store names of your Facebook connections. You can decide to withdraw your data from this study, but in this case we would not able to issue you the full compensation."),
        # renderTable(AllInputs() |>
        #               sample_n(1)
        #             # |> head(10)
        #             ),
        actionButton(inputId = "actionButton_submit1",
                   label = "Submit and continue"),
        actionButton(inputId = "actionButton_submit0",
                     label = "Remove all my data from the study.")
      )
    )
    
  
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_verification")
    
    
    
    # values$logs |> isolate() |> write.csv("x.csv")
    
  })
  
  observeEvent(input$actionButton_submit1, {
    hide("verification_div")
    
    
    
    output$text_token_out <- renderText({ values$validation_code })
    
    output$html_final <- renderUI(
      column(
        width = w,
        fluidRow(),
        h3("Thank your for the participation in the survey, please copy this code and paste it to the field above."),
        fluidRow(
          verbatimTextOutput("text_token_out")
        ),
        fluidRow(
          align = "right",
          rclipButton(
            inputId = "clipbtn",
            label = "Copy code",
            clipText = isolate(values$validation_code), 
            icon = icon("clipboard")
          )
        )
        
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_final_1")
    
  })
  
  observeEvent(input$actionButton_submit0, {
    hide("verification_div")
    
    values$remove <- TRUE
    
    output$html_final <- renderUI(
      column(
        width = w,
        fluidRow(),
        h3("Your answers were removed from the survey, feel free to close the tab."),
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_final_0")
    
  })
  


  
  onStop(function() {
    print("session ended")
    # values$logs |> isolate() |> write.csv("logs.csv")
    
    mongo_batch$insert(
      values$logs |> isolate()
    )
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "session_ended")
    # unlink("friends_and_followers/removed_friends.json")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
