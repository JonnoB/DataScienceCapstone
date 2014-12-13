library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predictification"),
  

  sidebarLayout(
    sidebarPanel( 
      helpText("Enter the phrase you wish to predict the last word for in the box below"),
      textInput("phrase", "Phrase to predict last word:", "My Phrase is..."),
      submitButton("Submit"),
      h2("Details"),
      p("The Predictifier, comes to it's conclusions using the following method. 
        It first cleans the sentence in two alternative ways. The first method removes all punctuation 
        (except for those used in contractions such as the apostraphe found in \"i'd like a sandwich\") and numbers. 
        The second method does the same but changes 
        either the last word or the last two words to '[blank]' to take account of new phrases or words "),
      p("It then compares the cleaned phrases to two similarly cleaned dictionaries of", a("ngrams", href= "http://en.wikipedia.org/wiki/N-gram")),
      p("Matching ngrams are compiled then sorted according to Ngram length tie then ordering on length probability,
          This is becuase although a lower Ngram has may have a higher probability it lacks context.
          Finally the resulting most likely ngram is then returned. For more information click", 
        a("here", href ="https://rpubs.com/JonnoB/47443"), 
        "to see an Rpub presentation.")
    ),
    
    mainPanel(
     h3(p("The Predictifier thinks your word is", align = "center")),
      h2(textOutput("phrase1"), align = "center"),
      p("which would make your sentence be '",strong( textOutput("phrase2", inline = TRUE)),
        "' Does this suggestion seem sensible to you?", align = "center"),
      h3(p("Table of alternatives")),
      p("The options available for this phrase are as follows, do any of these match your sentence better?"),
      
      tableOutput("view")

          )
  )
))