#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ShinyRatingInput)
library(activityinfo)
library(janitor)
library(tidyverse)

addResourcePath("www", "www")
activityInfoLogin(Sys.getenv('ACTIVITY_INFO_UN'),
                  Sys.getenv('ACTIVITY_INFO_TOKEN'),
                  savePassword = FALSE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script(src = "www/script.js"),
    tags$style(HTML("

 .symbol-filled {
   color: '#E1CC0D';
 }"))
  ),
  # Application title
  titlePanel("QUESTION SCORING SYSTEM"),
  
  # Show a plot of the generated distribution
  fluidRow(column(
    12,
    h1("Ask or give a score to a question"),
    HTML(
      "<p>A <strong>question voting system</strong> is a technique or procedure used to ascertain the group&#39;s desire to propose a solution to a specific problem or topic. This might entail voting on a proposal or decision, with each member casting a vote for or against the solution. Afterward, the votes are tabulated, and the <strong>winner is selected based on the majority vote</strong>.</p>
<p>This is an electronic voting system in which votes are cast and counted electronically instead of on paper ballots. It is used to increase the speed and accuracy of the voting process and to guarantee the results are visible and verifiable.</p>"
    ),
    wellPanel(
      selectInput("variable",
                  "Select the question to score:",
                  ""),
      p("Rate this Question"),
      ratingInput(
        "questionRating",
        label = "",
        dataStop = 10,
        dataFractions = 2,
        dataFilled = "fa fa-bell fa-2x",
        dataEmpty = "fa fa-bell-o fa-2x"
      )
    )
  )),
  fluidRow(
    column(
      3,
      offset = 6,
      align = "right",
      actionButton("add_new_question", label = "Add New Question", class =
                     "btn btn-primary text-bg-primary")
    ),
    column(
      3,
      align = "right",
      actionButton("save_score", label = "Save the score", class = "btn btn-primary text-bg-primary")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  initialized = reactiveVal(FALSE)
  observe({
    if (is.null(input$cookies$session_id)) {
      msg <- list(value = session$token)
      session$sendCustomMessage("session-set", msg)
    }
    print(input$cookies$session_id)
    initialized(TRUE)
  })
  questions <- reactive({
    req(initialized)
    queryTable(Sys.getenv('QUESTIONS_FORM_ID'),
               "Question" = "question",
               "Active" = "active", truncate.strings = FALSE) %>% janitor::clean_names() %>% filter(active == "Yes")
    
  })
  
  # save
  observeEvent(input$save_score, {
    msg <- list(name = input$variable,
                value = input$questionRating)
    
    if (input$variable != "")
      session$sendCustomMessage("cookie-set", msg)
  })
  
  observe({
    updateSelectInput(session, "variable",
                      label = "Select the question to score:",
                      choices = questions()$question,
                      selected = questions()$question[1])
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
