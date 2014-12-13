library(shiny)

options(shiny.maxRequestSize = 15*1024^2)
#packages <- c("R.utils", "tm", "dplyr", "stringr", "xtable") #
#invisible(lapply(packages, library, character.only = TRUE))
library(R.utils);library(tm); library(dplyr);library(stringr)

load("data/ngram_probs_blank_small.Rda")
load("data/ngram_probs_small.Rda")

corpus_clean<- function(x) {
  x <- tolower(x)
  x <- gsub("&", "and", x)
  x <- gsub("p m|p.m", "pm", x)
  x <- gsub("'ll", " will", x)
  x <- gsub("'d", " would", x)
  x <- gsub("'m", " am", x)
  x <- gsub("'ve", " have", x)
  x <- gsub("'re", " are", x)
  x <- gsub("can't|cannot|cant", "can not", x)
  x <- gsub("n't", " not", x)
  x <- gsub("'s", "", x)
  x <- removeNumbers(x)
  x <- removePunctuation(x)
  x <- gsub(" will", "''ll", x)
  x <- gsub(" would", "'d", x)
  x <- gsub(" am", "'m", x)
  x <- gsub(" have", "'ve", x)
  x <- gsub(" are", "'re", x)
  x <- gsub("cannot", "can't", x)
  x <- gsub(" not", "n't", x)
  x <- stripWhitespace(x)
}


result <- function(phrase, ngram_probs) { 
  phrase <- str_trim(phrase, side = "both")
  
  wordcount <- if(length(ngram_probs) < str_count(phrase, "\\S+")) {
    length(ngram_probs)
  } else  {str_count(phrase, "\\S+")}
  
  forNgrams <- sapply(1:wordcount, function(n) word(phrase, -n, -1))
  results <-lapply(1:wordcount, function(n) filter(ngram_probs[[n]], 
                                                   predictor == forNgrams[[n]]))
  results <- do.call(rbind.data.frame, results)
  results <- ungroup(results) %>%
    arrange( desc(Prob), desc(Ngrams))
  results
}

result_blank <- function(phrase, ngram_probs) { 
  phrase <- str_trim(phrase, side = "both")
  
  wordcount <- if(length(ngram_probs) < str_count(phrase, "\\S+")) {
    length(ngram_probs)
  } else  {str_count(phrase, "\\S+")}
  
  forNgrams <- sapply(1:wordcount, function(n) word(phrase, -n))
  results <-lapply(1:wordcount, function(n) filter(ngram_probs[[n]], 
                                                   predictor == forNgrams[[n]] | predictor == "[blank]"))
  results <- rbind_all(results)
  results <- ungroup(results) %>%
    arrange( desc(Prob), desc(Ngrams))
  results
}

shinyServer(function(input, output) {
  
  output$phrase1 <- renderText({
    phrase <- corpus_clean(input$phrase)
 
    results <- rbind(result_blank(phrase, ngram_probs_blank_small), 
                     result(phrase, ngram_probs_small)) %>%
      arrange( desc(Ngrams), desc(Prob))
    results <- toupper(as.character(results$dependent[1]))
  })
  output$phrase2 <- renderText({
    phrase <- corpus_clean(input$phrase)
    
    results <- rbind(result_blank(phrase, ngram_probs_blank_small), 
                     result(phrase, ngram_probs_small)) %>%
      arrange( desc(Ngrams), desc(Prob))
    results <- as.character(results$dependent[1])
    c(input$phrase, results)
  })
  output$view <- renderTable({ 
    phrase <- corpus_clean(input$phrase)
    results <- rbind(result_blank(phrase, ngram_probs_blank_small), 
                     result(phrase, ngram_probs_small)) %>%
      arrange( desc(Ngrams), desc(Prob))
    as.data.frame(results) #some sort of dplyr tablerender bug means that as.data.frame needs to be called.
  
  })
  
  
  })
