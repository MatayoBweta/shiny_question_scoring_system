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
library(waiter)
library(gt)
library(bs4Dash)

addResourcePath("www", "www")


ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dashboardHeader(title = dashboardBrand(
    title = "QUESTION SCORING SYSTEM",
    
    color = "primary",
    href = "https://rstudio.unhcr.org/iraq/3rp-planning/",
    image = "www/logo.png"
  ),
  titleWidth = "500px"),
  dashboardSidebar(sidebarUserPanel(
    image = "www/logo.png",
    name = "Welcome to QSS!"
  )),
  dashboardBody(
    useWaiter(),
    waiterOnBusy(html = tagList(spin_circle_square(),
                                "Processing ...")),
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
 }")),
      tags$style(
        HTML(
          ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }"
        )
      )
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(solidHeader = TRUE, status = "warning", title = "Ask or give a score to a question",width = 12,
          HTML("<p>A <strong>question scoring system</strong> is a technique or procedure used to ascertain the group&#39;s desire to propose a solution to a specific problem or topic. This might entail voting on a proposal or decision, with each member casting a vote for or against the solution. Afterward, the votes are tabulated, and the <strong>winner is selected based on the majority vote</strong>.</p>
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
      ), actionButton("add_new_question", label = "Add New Question", class =
                        "btn btn-primary text-bg-primary"),
      actionButton("save_score", label = "Save the score", class = "btn btn-primary text-bg-primary")
      ),
      
      box(solidHeader = TRUE, status = "warning",
        title = "Questions Scores",width = 12,
        gt_output("questions_scores")
      )
    )
  ),footer = dashboardFooter(
    left = a(
      href = "https://www.unhcr.org/iraq.html",
      target = "_blank", "UNHCR IRAQ"
    ),
    right = "2023"
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  initialized = reactiveVal(FALSE)
  values <- reactiveValues()
  values$refresh_table <- TRUE
  
  observe({
    if (is.null(input$cookies$session_id)) {
      msg <- list(value = session$token)
      session$sendCustomMessage("session-set", msg)
    }
    print(input$cookies$session_id)
    activityInfoLogin(Sys.getenv('ACTIVITY_INFO_UN'),
                      Sys.getenv('ACTIVITY_INFO_TOKEN'),
                      savePassword = FALSE)
    initialized(TRUE)
  })
  
  output$questions_scores <- render_gt({
    req(initialized)  # <---- dependency on authentication result
     
     
    question_score() %>%
       group_by(question) %>% gt() %>% 
       tab_header(
         title = md("**QUESTIONS SCORES**"),
         subtitle = "DETAILS ON SCORES BY QUESTION"
       )%>%
       cols_move_to_end(columns = c(scoring_date, score)) %>%
       cols_label(
         score_id  = "Score ID",
         score  = "Score",
         session_id  = "Session ID",
         scoring_date  = "Scoring Date"
       ) %>%
       summary_rows(
         groups = TRUE,
         columns = c(score),
         fns = list(
           TOT = ~sum(.,na.rm = TRUE)),
         formatter = fmt_number
       ) %>%
       tab_options(
         row_group.background.color = "#00B398",
         row_group.border.top.color = "#8EBEFF",
         row_group.border.bottom.style = "none",
         table.width = "97%",
         column_labels.background.color = "#0072BC"
       ) %>%
       opt_all_caps()
  })
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function() {
    modalDialog(
      title = "Add new question",
      HTML(
        "<p>There are a few things to consider before adding a new question to a list of questions:</p>
<ol>
	<li>
	Make sure the question is relevant and appropriate for the context in which it will be used.
	</li>
	<li>
	Consider the purpose of the list of questions and ensure that the new question aligns with that purpose.
	</li>
	<li>
	Check for duplicates and ensure that the new question is not already on the list.
	</li>
	<li>
	Consider the length and complexity of the question, as well as the level of knowledge or expertise required to answer it.
	</li>
	<li>
	If possible, review the list of questions with others to get feedback and ensure that the new question fits well with the rest of the list.
	</li>
</ol>
<p>Overall, it&#39;s important to carefully consider the context and purpose of the list of questions, and to ensure that the new question is relevant, appropriate, and well-suited to the needs of the audience.</p>"
      ),
      textAreaInput(
        "question_new",
        "Add new question to the dataset",
        placeholder = 'Question to add',
        width = "100%",
        rows = 2
      ),
      textAreaInput(
        "question_description_new",
        "Add new question description",
        placeholder = 'Description of the question to add',
        width = "100%",
        rows = 4
      ),
      footer = tagList(modalButton("Cancel"),
                       actionButton("ok", "OK"))
    )
  }
  
  questions <- reactive({
    req(initialized)
    queryTable(
      Sys.getenv('QUESTIONS_FORM_ID'),
      "id" = "_id",
      "Question" = "question",
      "Description" = "question_description",
      "Active" = "active",
      truncate.strings = FALSE
    )%>% janitor::clean_names() %>% mutate(question_ = question) %>% remove_rownames() %>% column_to_rownames(var = "question_") %>%  filter(active == "Yes")
    
  })
  
  question_score <- reactive({
    req(initialized)
    values$refresh_table
    queryTable(
      Sys.getenv('QUESTIONS_SCORE_FORM_ID'),
      "id" = "_id",
      "Score ID" = "score_id",
      "Question" = "question.question",
      "Score" = "score",
      "Session ID" = "session_id",
      "Scoring Date" = "scoring_date",
      truncate.strings = FALSE
    )%>% janitor::clean_names() %>% remove_rownames() %>% column_to_rownames(var = "id")
  })
  
  # save
  observeEvent(input$save_score, {
    msg <- list(name = input$variable,
                value = input$questionRating)
   
    dd <-  queryTable(
      Sys.getenv('QUESTIONS_SCORE_FORM_ID'),
      "id" = "_id",
      "Score ID" = "score_id",
      "Question" = "question.question",
      "Score" = "score",
      "Session ID" = "session_id",
      "Scoring Date" = "scoring_date",
      filter = paste0(
        "question.question == '",
        input$variable,
        "' && session_id =='",
        input$cookies$session_id,
        "'"
      ),
      truncate.strings = FALSE
    )  %>% janitor::clean_names() %>% filter(!is.na(question))
    glimpse(dd)
    c_date <- format(Sys.Date(), format = "%Y-%m-%d")
    if (nrow(dd) > 0) {
      createAlert(
        id = "myalert",
        options = list(
          title = "Alert",
          closable = TRUE,
          width = 12,
          elevations = 4,
          status = "primary",
          content = "Alert content ..."
        ))
      
      df <- dd %>% transmute(id,score = input$questionRating, scoring_date = c_date) %>% filter(!is.na(score))
      if(nrow(df) > 0)
        importTable(
          formId = Sys.getenv('QUESTIONS_SCORE_FORM_ID'),
          data = df,
          recordIdColumn = "id"
        )
    } else {
      
      #add rows
      dc <-  questions()
      df <- data.frame(matrix(ncol = 5, nrow = 0))
      colnames(df) <-
        c("id", "question", "score", "session_id", "scoring_date")
      id <- dc[input$variable, "id"]
      df[1, ] <-
        c(cuid(),
          id,
          input$questionRating,
          input$cookies$session_id,
          c_date)
      cc <- df %>% filter(!is.na(score))
       if(nrow(cc) > 0)
          importTable(
            formId = Sys.getenv('QUESTIONS_SCORE_FORM_ID'),
            data = cc,
            recordIdColumn = "id"
          )
      
      
      
    }
    

    values$refresh_table <- !values$refresh_table
  })
  
  # save
  observeEvent(input$add_new_question, {
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (!is.null(input$question) && nzchar(input$question)) {
      df <- data.frame(matrix(ncol = 5, nrow = 0))
      colnames(df) <-
        c("id", "question", "question_description", "active")

      df[1, ] <-
        c(cuid(),
          input$question_new,
          input$question_description_new,
          "Yes")
      df <- df %>% filter(!is.na(question))
      if(nrow(df) > 0)
        importTable(
          formId = Sys.getenv('QUESTIONS_FORM_ID'),
          data = df,
          recordIdColumn = "id"
        )
      removeModal()
    }
  })
  
  observe({
    req(initialized)  # <---- dependency on authentication result
    df <-  questions() 
    
    updateSelectInput(
      session,
      "variable",
      label = "Select the question to score:",
      choices = df$question,
      selected = df$question[1]
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
