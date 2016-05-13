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

  updateButton = function(index) {
    output[[paste0("dyn_button", index)]] <<- renderUI({
      actionButton(paste0("word", index), label = last_predictions[index],
                   width = button.width)
    })
  }

  updateOutput = function() {
    sapply(1:12, updateButton)
    output$predictedWord <- renderText(last_predictions[1])
  }

  # create the buttons (show them for the first time)
  updateOutput()

  addButtonClickHandler = function(button.index) {

    observeEvent(input[[paste0("word", button.index)]], {
      c_userText = input$userText
      new_text =
        updateTextInput(session, "userText",
                        value = paste(c_userText,
                                      last_predictions[button.index])
        )
      updateButton(button.index)
    })
  }

  # with a for(i in 1:12) style loop things blew up probably due to promise
  # evaluation, had to get the button.index evaluated, then it worked
  #
  # with sapply it just works
  sapply(1:12, addButtonClickHandler)

  observeEvent(input$userText, {
    c_userText = input$userText
    last_predictions <<- predict(c_userText)
    updateOutput()
  })
})

# some generated sentences:
#
# "I went to the bathroom and kitchen for the next week."
# "and I was so happy that you enjoyed the article."
#
# a hint of success
