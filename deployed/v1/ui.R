library(shiny)

shinyUI(fluidPage(
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
        #value = "Hello world!",
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
    column(4), align = "center",
    column(4,
           h5("Click buttons below to insert prediction")
    ),
    column(5)
  ),
  fluidRow(
    column(3, uiOutput("dyn_button1")),
    column(3, uiOutput("dyn_button2")),
    column(3, uiOutput("dyn_button3")),
    column(3, uiOutput("dyn_button4"))
  ),
  fluidRow(
    column(4), align = "center",
    column(4,
      h5("Associative prediction")
    ),
    column(4)
  ),
  fluidRow(
    column(3, uiOutput("dyn_button5")),
    column(3, uiOutput("dyn_button6")),
    column(3, uiOutput("dyn_button7")),
    column(3, uiOutput("dyn_button8"))
  ),
  fluidRow(
    column(3, uiOutput("dyn_button9")),
    column(3, uiOutput("dyn_button10")),
    column(3, uiOutput("dyn_button11")),
    column(3, uiOutput("dyn_button12"))
  )
))
