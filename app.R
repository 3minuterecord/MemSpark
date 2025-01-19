library(shiny)
library(shinythemes)
library(dplyr)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$style(rel = "stylesheet", type = "text/css", href = "custom.css"),
  theme = shinytheme("cyborg"),
  div(titlePanel(
    tagList(
      icon("lightbulb", style = "color:white;margin-left:15px;margin-top:15px;"), 
      span("MemSpark", style = "color:white;font-family: Tahoma, sans-serif;font-weight: 600;"),
      span("+", style = "color:gold;font-family: Tahoma, sans-serif;font-weight: 600;margin-left:-10px;margin-right:20px;"),
      span("100", class = "score-bubble-100"),
      span("87", class = "score-bubble"),
      span("92", class = "score-bubble"),
      span("75", class = "score-bubble"),
      span("68", class = "score-bubble"),
      div("Sharpen, Spark, Succeed.", style = "color:gold;margin-left:15px;margin-top:10px;font-size: 14px;font-weight: 200;")
    ), windowTitle = "Quiz Me"
  )),
  sidebarLayout(
    sidebarPanel(
      uiOutput("topic_selector"),
      div(actionButton("new_question", "New Test", icon = icon("rocket", style = "padding-right: 5px;")),  uiOutput('display_score', style = "display: inline-block; margin-left:10px;vertical-align: middle; color: white;font-size: 22px;font-weight: 800;")),
      uiOutput("progress_bar")
    ),
    
    mainPanel(
      uiOutput("area_tag"),
      div(id = "animated-text", "", style = 'margin-left:25px;margin-right:25px;'),
      uiOutput("show_answer_btn"),
      div(uiOutput("answer"), style = 'margin-left:25px;margin-top:10px;')
    )
  )
)

server <- function(input, output, session) {
  DEBUG <- FALSE
  test_num_of_questions <- 3
  latest_answer <- reactiveValues(ans='')
  show_buttons <- reactiveValues(status='none')
  show_ask_next_button <- reactiveValues(status='none')
  show_ans_button <- reactiveValues(status='none')
  show_ans_icon <- reactiveValues(status='none')
  show_tags <- reactiveValues(status='none')
  show_completion_flag <- reactiveValues(status='none')
  next_question <- reactiveValues(num=1)
  num_questions_asked <- reactiveValues(num=0)
  question_counter <- reactiveValues(val=0)
  quiz_score <- reactiveValues(val=0)
  question_nums <- reactiveValues(selected=NA)
  
  quiz_data <- reactive({
    data <- suppressWarnings(read.csv("data/knowledgebase.csv", stringsAsFactors = FALSE))
    shuffled_data <- data[sample(nrow(data)), ]
    return(shuffled_data)
  })
  
  output$topic_selector <- renderUI({
    div(selectInput("num", "Select Topic:", choices = c("Mix", sort(unique(quiz_data()$area)))))
  })
  
  
  observe({
    data <- quiz_data()
    total_num_of_questions = length(data$question)
    #set.seed(123)
    random_numbers <- sample(1:total_num_of_questions, test_num_of_questions, replace = FALSE)
    question_nums$selected <- random_numbers
    if (DEBUG){print(paste0("Randomly selected questions: ", paste(random_numbers, collapse = ", ")))}
  })
  
  observeEvent(input$new_question, {
    data <- quiz_data()
    question <- data$question[question_nums$selected[next_question$num]]
    latest_answer$ans <- '' 
    show_ask_next_button$status <- 'none'
    show_tags$status <- 'inline-block'
    session$sendCustomMessage("animateText", list(text = question, speed = 30)) 
    question_counter$val <- question_counter$val + 1
  })
  
  observeEvent(input$ask_new_question, {
    question <- quiz_data()$question[question_nums$selected[next_question$num]]
    latest_answer$ans <- '' 
    show_buttons$status <- 'none'
    show_tags$status <- 'inline-block'
    session$sendCustomMessage("animateText", list(text = question, speed = 30)) 
    show_ask_next_button$status <- 'none'
    show_ans_icon$status <- 'none'
    question_counter$val <- question_counter$val + 1
  })
  
  observeEvent(input$show_answer, {
    session$sendCustomMessage("hideAnsAsk", list())
    data <- quiz_data()
    latest_answer$ans <- data$answer[question_nums$selected[next_question$num]]
    show_ans_button$status <- 'none !important'
    show_buttons$status <- 'inline-block'
    show_ans_icon$status <- 'inline-block'
    if (next_question$num < test_num_of_questions){
      next_question$num <- next_question$num + 1
    }
  })
  
  observeEvent(input$show_yes, {
    session$sendCustomMessage("showClapping", list())
    Sys.sleep(1)
    show_buttons$status <- 'none'
    quiz_score$val <- quiz_score$val + 1
    num_questions_asked$num <- num_questions_asked$num + 1
    if (num_questions_asked$num == test_num_of_questions){
      show_completion_flag$status <- 'inline-block'
    } else {
      show_ask_next_button$status <- 'inline-block'  
    }
    
  })
  
  observeEvent(input$show_no, {
    session$sendCustomMessage("showSadFace", list())
    Sys.sleep(1)
    show_buttons$status <- 'none'
    num_questions_asked$num <- num_questions_asked$num + 1
    show_ask_next_button$status <- 'inline-block'
  })
  
  output$show_answer_btn <- renderUI({
    actionButton("show_answer", "Show Answer", icon = icon("eye", style = "padding-right: 5px;"), style = paste0("display: ", show_ans_button$status, "; margin-left:25px;margin-top:25px;margin-right:25px;"))
  })
  
  output$answer <- renderUI({
    div(
      div(icon("check-circle", style = paste0("display: ", show_ans_icon$status, "; color: green; margin-right:5px;")), latest_answer$ans),
      div(actionButton("show_yes", "Correct", icon = icon("circle-check", style = "padding-right: 3px;"), style = paste0("display: ", show_buttons$status, "; margin-left:0px;margin-top:25px;")), 
          actionButton("show_no", "Wrong", icon = icon("circle-xmark", style = "padding-right: 3px;"), style = paste0("display: ", show_buttons$status, "; margin-left:10px;margin-top:25px;")),
          div(id = "sad_icon", icon("sad-cry", style = "font-size: 24px; color:red;animation: pulse 0.5s linear infinite;"), style = "display: none;margin-left:9px;vertical-align:bottom;margin-bottom:5px;"),
          div(id = "clap_icon", icon("hands-clapping", style = "font-size: 24px; color:green;animation: pulse 0.5s linear infinite;"), style = "display: none;margin-left:9px;vertical-align:bottom;margin-bottom:5px;"),
          actionButton("ask_new_question", "Next Question", icon = icon("forward", style = "padding-right: 4px;"), style = paste0("display: ", show_ask_next_button$status, "; margin-left:0px;margin-top:25px;background-color: gold;opacity:0.5;color:black;"))
          ),
      div(icon("flag-checkered", style = paste0("display: ", show_completion_flag$status, "; white: green; margin-right:5px;font-size: 16px;")), span('Test Complete!', style = paste0("display: ", show_completion_flag$status, "; color: white; margin-top:35px;font-size: 16px;")))
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
  
  output$progress_bar <- renderUI({
    done = round(100 * (num_questions_asked$num / test_num_of_questions), 0)
    left = 99 - done
    div(
      div(style = paste0("width:", done, "%; height:25px;display: inline-block; margin-top:10px; margin-right:0px;background-color: green;")),
      div(style = paste0("width:", left, "%; height:25px;display: inline-block; margin-top:10px; margin-left:-3px;background-color: lightgrey;opacity: 0.2;background: repeating-linear-gradient( -45deg, grey, darkgrey, 1px, gold 2px, #e5e5f7 7px );"))
    )
  })
  
  output$area_tag <- renderUI({
    out_area <- quiz_data()$area[question_nums$selected[question_counter$val]]
    out_topic <- quiz_data()$topic[question_nums$selected[question_counter$val]]
    out_subtopic <- quiz_data()$subtopic[question_nums$selected[question_counter$val]]
    div(
      div(out_area, style= paste0("display: ", show_tags$status, "; padding:5px;padding-right:8px;padding-left:8px;
                                margin-left:25px;color:black;margin-top:5px;margin-bottom:15px;background-color:gold;
                                opacity: 0.4;border-radius:4px;animation: fadeIn 1s;")),
      div(out_topic, style= paste0("display: ", show_tags$status, "; padding:5px;padding-right:8px;padding-left:8px;
                              margin-left:5px;color:black;margin-top:5px;margin-bottom:15px;background-color:silver;
                              opacity: 0.45;border-radius:4px;animation: fadeIn 1s")),
      div(out_subtopic, style= paste0("display: ", show_tags$status, "; padding:5px;padding-right:8px;padding-left:8px;
                              margin-left:5px;color:black;margin-top:5px;margin-bottom:15px;background-color:olive;
                              opacity: 0.8;border-radius:4px;animation: fadeIn 1s"))
    )
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
      }, 800); // 2-second delay
    }
  }
  
  typeWriter();
});

Shiny.addCustomMessageHandler('hideAnsAsk', function(data) {
  let answerBtn = document.getElementById('show_answer');
  answerBtn.style.display = 'none'; 
});

Shiny.addCustomMessageHandler('showSadFace', function(data) {
  const sadIcon = document.getElementById('sad_icon');
  sadIcon.style.display = 'inline-block'; // Show the icon
  setTimeout(() => {
    sadIcon.style.display = 'none'; // Hide the icon after 2 seconds
  }, 4000);
});

Shiny.addCustomMessageHandler('showClapping', function(data) {
  const sadIcon = document.getElementById('clap_icon');
  sadIcon.style.display = 'inline-block'; // Show the icon
  setTimeout(() => {
    sadIcon.style.display = 'none'; // Hide the icon after 2 seconds
  }, 4000);
});

"

ui <- tagList(
  ui,
  tags$script(HTML(jsCode))
)

shinyApp(ui, server)
