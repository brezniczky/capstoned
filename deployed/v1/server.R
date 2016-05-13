library(shiny)

source("clean/solution/16_combined_prediction.R")

intialize.combined.pred.data()

predict = function(text) {
  combined.pred.data = intialize.combined.pred.data()

  words = get_words(text = text, standardize = TRUE)
  ids = words.to.ids(words, combined.pred.data$dict.by.word)

  # only consider things in the sentence being written
  n.ids = length(ids)
  if (n.ids > 0) {
    for(i in (n.ids:1))
      if (is.na(ids[i])) {
        ids = tail(ids, n.ids - i)
        break
      }
  }

  preds = combined.predict(word_ids = ids, # tail(ids, 4),
                           pred.data = combined.pred.data)

  return(
    combined.pred.data$dict.by.id[.(preds$word_id)]$word # [1:4]
  )
}

shinyServer(function(input, output, clientData, session) {

  last_predictions = c("1", "2", "3", "4")
  button.width = "200px"

  # TODO: refactor - create one updateButton function
  updateButton1 = function() {
    output$dyn_button1 <<- renderUI({
      actionButton("word1", label = last_predictions[1], width = button.width)
    })
  }

  updateButton2 = function() {
    output$dyn_button2 <- renderUI({
      actionButton("word2", label = last_predictions[2], width = button.width)
    })
  }

  updateButton3 = function() {
    output$dyn_button3 <- renderUI({
      actionButton("word3", label = last_predictions[3], width = button.width)
    })
  }

  updateButton4 = function() {
    output$dyn_button4 <- renderUI({
      actionButton("word4", label = last_predictions[4], width = button.width)
    })
  }

  updateButton5 = function() {
    output$dyn_button5 <- renderUI({
      actionButton("word5", label = last_predictions[5], width = button.width)
    })
  }

  updateButton6 = function() {
    output$dyn_button6 <- renderUI({
      actionButton("word6", label = last_predictions[6], width = button.width)
    })
  }

  updateButton7 = function() {
    output$dyn_button7 <- renderUI({
      actionButton("word7", label = last_predictions[7], width = button.width)
    })
  }

  updateButton8 = function() {
    output$dyn_button8 <- renderUI({
      actionButton("word8", label = last_predictions[8], width = button.width)
    })
  }
  updateButton9 = function() {
    output$dyn_button9 <- renderUI({
      actionButton("word9", label = last_predictions[9], width = button.width)
    })
  }
  updateButton10 = function() {
    output$dyn_button10 <- renderUI({
      actionButton("word10", label = last_predictions[10], width = button.width)
    })
  }
  updateButton11 = function() {
    output$dyn_button11 <- renderUI({
      actionButton("word11", label = last_predictions[11], width = button.width)
    })
  }
  updateButton12 = function() {
    output$dyn_button12 <- renderUI({
      actionButton("word12", label = last_predictions[12], width = button.width)
    })
  }

  updateAllButtons = function() {
    updateButton1()
    updateButton2()
    updateButton3()
    updateButton4()
    updateButton5()
    updateButton6()
    updateButton7()
    updateButton8()
    updateButton9()
    updateButton10()
    updateButton11()
    updateButton12()
    output$predictedWord <- renderText(last_predictions[1])
  }

  # create the buttons (show them for the first time)
  updateAllButtons()

  observeEvent(input$word1, {
    c_userText = input$userText
    c_word4 = input$word1
    new_text =
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[1])
    )
    updateButton1()
  })

  observeEvent(input$word2, {
    c_userText = input$userText
    c_word4 = input$word2
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[2])
    )
    updateButton2()
  })

  observeEvent(input$word3, {
    c_userText = input$userText
    c_word4 = input$word3
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[3])
    )
    updateButton3()
  })

  observeEvent(input$word4, {
    c_userText = input$userText
    c_word4 = input$word4
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[4])
    )
    updateButton4()
  })

  observeEvent(input$word5, {
    c_userText = input$userText
    c_word5 = input$word5
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[5])
    )
    updateButton5()
  })

  observeEvent(input$word6, {
    c_userText = input$userText
    c_word4 = input$word6
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[6])
    )
    updateButton6()
  })

  observeEvent(input$word7, {
    c_userText = input$userText
    c_word4 = input$word7
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[7])
    )
    updateButton7()
  })

  observeEvent(input$word8, {
    c_userText = input$userText
    c_word4 = input$word8
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[8])
    )
    updateButton8()
  })

  observeEvent(input$word9, {
    c_userText = input$userText
    c_word4 = input$word9
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[9])
    )
    updateButton9()
  })

  observeEvent(input$word10, {
    c_userText = input$userText
    c_word4 = input$word10
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[10])
    )
    updateButton10()
  })

  observeEvent(input$word11, {
    c_userText = input$userText
    c_word4 = input$word11
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[11])
    )
    updateButton11()
  })

  observeEvent(input$word12, {
    c_userText = input$userText
    c_word4 = input$word12
    updateTextInput(session, "userText",
                    value = paste(c_userText, last_predictions[12])
    )
    updateButton8()
  })

  observeEvent(input$userText, {
    c_userText = input$userText
    last_predictions <<- predict(c_userText)
    updateAllButtons()
  })
})
