#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
#
# fss14142
    
    # Application title
    titlePanel("Assignment 10"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        
        sidebarPanel(
            
            br(),
            h5("Please enter some / a word for the prediction of the next word"),
            textInput(inputId = "user_input", 
                      label = "", 
                      value = "" ) 
            ),


        mainPanel(
            

                tabPanel(
                    "Prediction",
                    h4("The predicted word (with highest probability) is:"),
                    HTML("<span style='color:red'>"),
                    h3(textOutput("predictedWordMain"), align="center"),
                    HTML("</span>"),
                    br(),
#                    h4(textOutput("kText")),
                    h4(textOutput("pred_text")),
                    hr()
                )
            )       
        )
    )
)
