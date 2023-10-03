
library(shiny)
library(tidyverse)
library(jsonlite)
library(purrr)
library(colorspace)
library(shinyjs)
library(digest)
library(shinyWidgets)

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

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Unfriending on Facebook"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          HTML("
               <b> Instructions and opt-out: </b> <br>
               In the following survey, we will ask questions about 
               up to eight of your most recently removed Facebook 
               connections, and up to eight of your current connections. 
               If there are any individuals that you wish to avoid 
               discussing, please uncheck their names here.
               ")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidPage(
            tags$script(
              "$(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'a') {
            $('#valueA').text(event.value);
          }
        });
        "
            ),
          div(id = "upload_deleted_div",
              fileInput("upload_deleted", "Upload a facebook file",
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
                           logs = NULL)
  
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
          h4("Sorry, you have not deleted any friends in the past 3 years, and therefore you are not qualify for the next part of the study")
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
          label = "Selected deleted friends",
          choices = c(values$fb_df_existing$name, values$fb_df_removed$name),
          selected  = c(values$fb_df_existing$name, values$fb_df_removed$name)
        ),
        actionButton(inputId = "actionButton_fb_confirm",
                     label = "Confirm")
      )
    )
  }
    
  
    
    
  })
  
  
  
  observeEvent(input$actionButton_fb_confirm, {
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
  })
  
  observeEvent(input$actionButton1_verify, {
    hide("questions_p_div")
    
    ## TODO: replace with loop map?
    
    # answers_1 <- tibble(
    #   id = 1,
    #   question = c("input$radioButtons_political1", 
    #                "input$radioButtons_political2"),
    #   answer = c(input$radioButtons_political1_1, 
    #              input$radioButtons_political2_1)
    #   )
    # 
    # answers_2 <- tibble(
    #   id = 2,
    #   question = c("input$radioButtons_political1", 
    #                "input$radioButtons_political2"),
    #   answer = c(input$radioButtons_political1_2, 
    #              input$radioButtons_political2_2)
    #   )
    # 
    # answers_all <- answers_1 |> 
    #   bind_rows(answers_2)
    
    
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
      values$logs <- x
      x
    })
    
    # output$show_inputs <- renderTable({
    #   AllInputs()
    # })
    
    # output_df <- isolate(values$fb_df_filtered) |> 
    #   left_join(answers_all) |> 
    #   rowwise() |> 
    #   mutate(name = map_chr(name, digest, algo = 'md5')) |> 
    #   mutate(user = session$token) |> 
    #   select(user, name, timestamp,question, answer)
    
    output$html_verification <- renderUI(
      column(
        width = 8,
        renderTable(AllInputs() 
                    # |> head(10)
                    ),
        actionButton(inputId = "actionButton_sumbit1",
                   label = "Sumbit"),
        actionButton(inputId = "actionButton_sumbit0",
                     label = "Remove all my data from the study delete facebook")
      )
    )
    
    
  })
  
  observeEvent(input$actionButton_sumbit1, {
    ## TODO: add end screen and do not kill the whole app
    # stopApp()
  })

  observeEvent(input$actionButton_sumbit0, {
    
    # stopApp()
  })
  


  
  onStop(function() {
    print("session ended")
    mongo_batch$insert(
      values$logs |> isolate()
    )
    # unlink("friends_and_followers/removed_friends.json")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
