library(shiny)
library(shinythemes)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #animated-text {
        font-size: 24px;
        font-family: Lucida Console, sans-serif;
        font-weight: 100;
        color: #d3d3d3;
        white-space: pre-wrap; /* Preserve spaces and line breaks */
      }
    "))
  ),
  theme = shinytheme("cyborg"),
  titlePanel("Quiz Me"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("num", "Select Topic:", choices = 1:10),
      actionButton("new_question", "New Question")
    ),
    
    mainPanel(
      div(id = "animated-text", ""),
      actionButton("show_answer", "Show Answer", style = "display: none;"), # Button initially hidden
      div(uiOutput("answer"))
    )
  )
)

server <- function(input, output, session) {
  
  latest_answer <- reactiveValues(ans='')
  
  quiz_data <- reactive({
    data <- read.csv("data/knowledgebase.csv", stringsAsFactors = FALSE)
    return(data)
  })
  
  observeEvent(input$new_question, {
    data <- quiz_data()
    question <- data$question[1]
    latest_answer$ans <- '' 
    session$sendCustomMessage("animateText", list(text = question, speed = 50)) 
  })
  
  observeEvent(input$show_answer, {
    data <- quiz_data()
    answer <- data$answer[1]
    latest_answer$ans <- data$answer[1]
  })
  
  output$answer <- renderUI({
    div(latest_answer$ans)
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
      answerBtn.style.display = 'inline-block'; // Show the button after animation
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
