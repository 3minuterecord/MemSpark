library(shiny)
library(shinythemes)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$style(rel = "stylesheet", type = "text/css", href = "custom.css"),
  theme = shinytheme("cyborg"),
  div(titlePanel(
    tagList(
      icon("lightbulb", style = "color:white;margin-left:15px;margin-top:15px;"), 
      span("MemSpark", style = "color:white;font-family: Tahoma, sans-serif;font-weight: 600;"),
      span("+", style = "color:gold;font-family: Tahoma, sans-serif;font-weight: 600;margin-left:-10px;"),
      div("Sharpen, Spark, Succeed.", style = "color:gold;margin-left:15px;margin-top:10px;font-size: 14px;font-weight: 200;")
    ), windowTitle = "Quiz Me"
  )),
  sidebarLayout(
    sidebarPanel(
      uiOutput("topic_selector"),
      div(actionButton("new_question", "New Question"),  uiOutput('display_score', style = "display: inline-block; margin-left:10px;vertical-align: middle; color: white;font-size: 22px;font-weight: 800;"))
    ),
    
    mainPanel(
      div(id = "animated-text", "", style = 'margin-left:25px;'),
      actionButton("show_answer", "Show Answer", style = "display: none; margin-left:25px;margin-top:10px;"), # Button initially hidden
      div(uiOutput("answer"), style = 'margin-left:25px;margin-top:10px;')
    )
  )
)

server <- function(input, output, session) {
  DEBUG <- FALSE
  test_num_of_questions <- 10
  latest_answer <- reactiveValues(ans='')
  show_buttons <- reactiveValues(status='none')
  next_question <- reactiveValues(num=1)
  num_questions_asked <- reactiveValues(num=0)
  quiz_score <- reactiveValues(val=0)
  question_nums <- reactiveValues(selected=NA)
  
  quiz_data <- reactive({
    data <- suppressWarnings(read.csv("data/knowledgebase.csv", stringsAsFactors = FALSE))
    return(data)
  })
  
  output$topic_selector <- renderUI({
    div(selectInput("num", "Select Topic:", choices = sort(unique(quiz_data()$area))))
  })
  
  
  observe({
    topic <- 'Area'
    data <- quiz_data() %>% filter(topic == topic)
    total_num_of_questions = length(data$question)
    #set.seed(123)
    random_numbers <- sample(1:total_num_of_questions, test_num_of_questions, replace = FALSE)
    question_nums$selected <- random_numbers
    if (DEBUG){print(paste0("Randomly selected questions: ", paste(random_numbers, collapse = ", ")))}
  })
  
  observeEvent(input$new_question, {
    topic <- 'Management'
    data <- quiz_data() %>% filter(topic == topic)
    question <- data$question[question_nums$selected[next_question$num]]
    latest_answer$ans <- '' 
    session$sendCustomMessage("animateText", list(text = question, speed = 50)) 
    show_buttons$status <- 'none'
  })
  
  observeEvent(input$show_answer, {
    data <- quiz_data()
    latest_answer$ans <- data$answer[question_nums$selected[next_question$num]]
    show_buttons$status <- 'inline-block'
    if (next_question$num < test_num_of_questions){
      next_question$num <- next_question$num + 1
    }
  })
  
  observeEvent(input$show_yes, {
    quiz_score$val <- quiz_score$val + 1
    num_questions_asked$num <- num_questions_asked$num + 1
  })
  
  observeEvent(input$show_no, {
    num_questions_asked$num <- num_questions_asked$num + 1
  })
  
  output$answer <- renderUI({
    div(
      div(icon("check-circle", style = paste0("display: ", show_buttons$status, "; color: green; margin-right:5px;")), latest_answer$ans),
      div(actionButton("show_yes", "Correct", style = paste0("display: ", show_buttons$status, "; margin-left:0px;margin-top:25px;")), 
          actionButton("show_no", "Wrong", style = paste0("display: ", show_buttons$status, "; margin-left:10px;margin-top:25px;")))
    )
  })
  
  output$display_score <- renderUI({
    if (DEBUG){
      print(paste0("Qs asked: ", num_questions_asked$num))
      print(paste0("Score: ", quiz_score$val))
    }
    
    if (num_questions_asked$num == 0){
      score <- 0
    } else {
      score <- 100 * round(quiz_score$val / num_questions_asked$num, 2)
    }
    div(paste0(score, '%'))
  })
}

jsCode <- "
Shiny.addCustomMessageHandler('animateText', function(data) {
  let textDiv = document.getElementById('animated-text');
  let answerBtn = document.getElementById('show_answer');
  textDiv.textContent = ''; // Clear existing text
  let text = data.text;
  let i = 0;

  answerBtn.style.display = 'none'; // Hide the button during animation
  
  function typeWriter() {
    if (i < text.length) {
      textDiv.textContent += text.charAt(i);
      i++;
      setTimeout(typeWriter, data.speed);
    } else {
      setTimeout(() => {
        answerBtn.style.display = 'inline-block'; // Show the button after delay
      }, 1000); // 2-second delay
    }
  }
  
  typeWriter();
});
"

ui <- tagList(
  ui,
  tags$script(HTML(jsCode))
)

shinyApp(ui, server)
