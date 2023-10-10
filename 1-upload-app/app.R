
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

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Unfriending on Facebook"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          div(id = "intro_div",
          HTML("
               <b> Instructions and opt-out: </b> <br>
               In the following survey, we will ask questions about 
               up to eight of your most recently removed Facebook 
               connections, and up to eight of your current connections. 
               If there are any individuals that you wish to avoid 
               discussing, please uncheck their names here.
               ")
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidPage(
            rclipboardSetup(),
            tags$script(
              "$(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'a') {
            $('#valueA').text(event.value);
          }
        });
        "
            ),
          div(id = "upload_deleted_div",
              fileInput("upload_deleted", "Upload a facebook zip file",
                        accept = ".zip"),
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
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values <- reactiveValues(fb_df_removed = NULL,
                           fb_df_existing = NULL,
                           fb_df_filtered = NULL,
                           logs = NULL,
                           remove = FALSE)
  
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
      output$html_confirm_friends <- renderUI(
          h4("Sorry, you have not deleted any friends in the past 3 years, and therefore you are not qualify for the next part of the study"),
          
          track_that(mongo_batch_track = mongo_batch_track, 
                     time = Sys.time(),
                     session = session$token,
                     stage = "html_confirm_friends_0")
      )
    } else {
      
  
      
      fb_removed_df <- fb_removed_df |>
        unnest_wider(col = deleted_friends_v2) |>
        mutate(timestamp = timestamp |>
                 as.Date.POSIXct())
      
    values$fb_df_removed <- fb_removed_df |>
      head(10) |>
      mutate(id = row_number()) |>
      mutate(bg_color = n() |>
               purple_colfunc() |>
               lighten(amount = 0.75))
    
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
      sample_n(size = nrow(fb_removed_df)) |> ## TODO: replace with 5?
      mutate(id = row_number()) |>
      mutate(bg_color = n() |>
               purple_colfunc() |>
               lighten(amount = 0.75))


    output$html_confirm_friends <- renderUI(
      column(
        width = 8,
        checkboxGroupInput(
          inputId = "checkboxGroupInput_fb_cpnfirm",
          label = "Please, uncheck names that you do not feel comfortable talking about.",
          choices = c(values$fb_df_existing$name, values$fb_df_removed$name),
          selected  = c(values$fb_df_existing$name, values$fb_df_removed$name)
        ),
        actionButton(inputId = "actionButton_fb_confirm",
                     label = "Confirm")
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_confirm_friends_1")
  }
    
  
    
    
  })
  
  
  
  observeEvent(input$actionButton_fb_confirm, {
    hide("intro_div")
    hide("upload_deleted_div")
    
    values$fb_df_removed_filtered <- isolate(values$fb_df_removed) |> 
      filter(name %in% input$checkboxGroupInput_fb_cpnfirm)

    if(nrow(values$fb_df_removed_filtered) < 1){
      ## TODO: message or something
      output$html_deleted <- renderUI(
        H4("No removed friends left :(")
      )

    } else {
      output$html_deleted <- renderUI(
        column(
          width = 8,
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
      filter(name %in% input$checkboxGroupInput_fb_cpnfirm)
    
    if(nrow(values$fb_df_existing_filtered) < 1){
      ## TODO: message or something
      output$html_deleted <- renderUI(
        H4("No existing friends left :(")
      )
      
    } else {
      output$html_existing <- renderUI(
        column(
          width = 8,
          fluidRow(
            isolate(values$fb_df_removed_filtered) |>
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
        width = 8,
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
    
    
    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
      
      x <- x |> 
        as_tibble() |> 
        t() |> 
        data.frame() |> 
        rownames_to_column(var = "input") |> 
        pivot_longer(cols = -'input', names_to = "user") |> 
        mutate(
          session_user = session$user,
          time = Sys.time()
               )
      
       ## TODO: add time of friends
      if (values$remove == TRUE) {
        values$logs <- data.frame(
          input = "removed",
          session_user = session$user,
          time = Sys.time()
        )
      } else {
        values$logs <- x
        # x
      }
      

    })
    
    
    output$html_verification <- renderUI(
      column(
        width = 8,
        h4("An example of the anonymized data that we collect."),
        renderTable(AllInputs() |> 
                      sample_n(10)
                    # |> head(10)
                    ),
        actionButton(inputId = "actionButton_sumbit1",
                   label = "Sumbit"),
        actionButton(inputId = "actionButton_sumbit0",
                     label = "Remove all my data from the study.")
      )
    )
    
  
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_verification")
  })
  
  observeEvent(input$actionButton_sumbit1, {
    hide("verification_div")
    
    output$text_token_out <- renderText({ session$token })
    
    output$html_final <- renderUI(
      column(
        width = 8,
        fluidRow(),
        h4("Thank your for the participation in the survey, please copy this code and paste it to the qualtrics."),
        fluidRow(
          verbatimTextOutput("text_token_out")
        ),
        fluidRow(
          align = "right",
          rclipButton(
            inputId = "clipbtn",
            label = "Copy code",
            clipText = session$token, 
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
  
  observeEvent(input$actionButton_sumbit0, {
    hide("verification_div")
    
    values$remove <- TRUE
    
    output$html_final <- renderUI(
      column(
        width = 8,
        fluidRow(),
        h4("Your data was removed from the survey, feel free to close the tab."),
      )
    )
    
    track_that(mongo_batch_track = mongo_batch_track, 
               time = Sys.time(),
               session = session$token,
               stage = "html_final_0")
    
  })
  


  
  onStop(function() {
    print("session ended")
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
