#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#tri.freq <- read.csv("R/trigram_freq_table.csv", stringsAsFactors=FALSE)
#bi.freq <- read.csv("R/bigram_freq_table.csv", stringsAsFactors=FALSE)
#uni.freq <- read.csv("R/unigram_freq_table.csv", stringsAsFactors=FALSE)

#tri.freq[1]  <- list(NULL)  # remove the row number - save memory and more intuitive decrement/increment in algo
#bi.freq[1]  <- list(NULL)
#uni.freq[1] <- list(NULL)


# fss14142
shinyServer(function(input, output) {

  data_input <- reactive({
  input_words <- strsplit(tolower(input$user_input),"\\s+")[[1]]
  # input_words
  wc_f=length(input_words)
  wc_f
  }) 

  
  wc = data_input()
  
  done = FALSE
  
  while (wc >= 0) {
    
    if (done) {
#      print("Done at top of loop. break")
      break
    }
    
    if (wc == 2) {
      freq_table = tri.freq
      print("freq_table is tri")
    }
    else if (wc ==1) {
      freq_table = bi.freq
      print("freq_table is bi")
    } 
    else {
      uni.freq[1,1]   # output the most common unigram
      print ("uni printed")
      done <- TRUE
    }
    
    
    if (isFALSE(done)) {
      for (i in 1:nrow(freq_table)) {
        
        if (all(input_words == freq_table[i,1:wc])) {
          output <- freq_table[i,wc+1]
          print("matched to freq table word")
          print(i)
          print(wc+1)
          print(output)
          ###############
          # while matched and less than 10
          # display n+1 word (display 1st ten or less next words)
          done <- TRUE
          break # break from for loop
        }     
      }
    }
    
    wc <- wc - 1
    input_words <- input_words[-1]  # remove the 1st word from input and try again - stupid backoff mtd
    
  }
  
  
  
    
    output$pred_text <- renderText({
      data_input()
      #input$user_input
    })
})
