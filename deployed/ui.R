library(shiny)

shinyUI(fluidPage(
  theme = "janca.css",
  fluidRow(
    column(4),
    column(
      4, align = "center",

      titlePanel(title = "Word entry assistant",
                 windowTitle = "Janos Brezniczky Capstone"),
      h4("Proof of concept application"),
      h4("-"),
      h4("Janos Brezniczky, 2016"),
      h5("Capstone project"),
      h6("for the"),
      h5("Data Science Specialization"),
      h6("provided by"),
      h5("Coursera & the Johns Hopkins University")
    ),
    column(4)
  ),
  fluidRow(
    column(1),
    column(12,
      textInput(
        inputId = "userText",
        label = "Enter text here:",
        value = "I believe",
        width = "100%")
    ),
    column(1)
  ),
  fluidRow(
    column(4), align = "center",
    column(4, h5("Predicted word:")),
    column(5)
  ),
  fluidRow(
    column(4), align = "center",
    column(4, h3(textOutput("predictedWord"))),
    column(5)
  ),
  fluidRow(
    column(3),
    column(6, align="center",
           h3("Click buttons below to append predicted word")
    ),
    column(3)
  ),
  fluidRow(
    column(3),
    column(6, align="center",
           h5("N-gram based")),
    column(3)
  ),
  fluidRow(
    column(align = "center", width = 12,
           uiOutput("dyn_button1", inline = TRUE),
           uiOutput("dyn_button2", inline = TRUE),
           uiOutput("dyn_button3", inline = TRUE),
           uiOutput("dyn_button4", inline = TRUE)
    )
  ),
  fluidRow(
    column(3),
    column(6, align="center",
           h5("Associative")),
    column(3)
  ),
  fluidRow(
    column(align = "center", width = 12,
           uiOutput("dyn_button5", inline = TRUE),
           uiOutput("dyn_button6", inline = TRUE),
           uiOutput("dyn_button7", inline = TRUE),
           uiOutput("dyn_button8", inline = TRUE)
    )
  ),
  fluidRow(
    column(align = "center", width = 12,
           uiOutput("dyn_button9" , inline = TRUE),
           uiOutput("dyn_button10", inline = TRUE),
           uiOutput("dyn_button11", inline = TRUE),
           uiOutput("dyn_button12", inline = TRUE)
    )
  )
))
