#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)


# Define UI for application that draws a histogram
ui<- fluidPage(
  
  
  titlePanel ('Predicting pointspergame'),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Free_Throw_Percentage",
                  "Select Free_Throw_Percentage:",
                  min = 0,
                  max = 1,
                  value = 0.71),
      sliderInput("Field_Goal_Percentage",
                  "Select Field_Goal_Percentage:",
                  min = 0,
                  max = 1,
                  value =0.44),
      sliderInput("Minutes_Played_Per_Game",
                  "Select Minutes_Played_Per_Game:",
                  min = 0,
                  max = 41,
                  value =17.60),
      
      sliderInput("Assist_Per_Game",
                  "Select Assist_Per_Game:",
                  min = 0,
                  max = 11,
                  value =1.58))
   
    ,
    
  # Show a plot of the generated distribution
  mainPanel( textOutput("prediction"))))
                
                
                server <- function(input, output){
                  # pass our inputs to our prediction function defined earlier
                  # and pass that result to the output

                
                  
                  output$prediction <- renderText({
                      data <-read.csv('418final.csv')
                  
                      data$Free_Throw_Percentage[is.na(data$Free_Throw_Percentage)] <- mean(data$Free_Throw_Percentage, na.rm = TRUE)
                      data$Field_Goal_Percentage[is.na(data$Field_Goal_Percentage)] <- mean(data$Field_Goal_Percentage, na.rm = TRUE)
                      data$Minutes_Played_Per_Game[is.na(data$Minutes_Played_Per_Game)] <- mean(data$Minutes_Played_Per_Game, na.rm = TRUE)
                      data$Assist_Per_Game[is.na(data$Assist_Per_Game)] <- mean(data$Assist_Per_Game, na.rm = TRUE)
                
                  
                  fit <- lm(Points_Per_Game ~ Free_Throw_Percentage+Field_Goal_Percentage+
                              Minutes_Played_Per_Game+Assist_Per_Game,data=data)
                  
                  preds <- function(fit, Free_Throw_Percentage, Field_Goal_Percentage, Minutes_Played_Per_Game, Assist_Per_Game){
                    
                    PointsPerGame <- predict(object=fit, 
                                             newdata = data.frame(
                                               Free_Throw_Percentage=Free_Throw_Percentage, 
                                               Field_Goal_Percentage=Field_Goal_Percentage, 
                                               Minutes_Played_Per_Game=Minutes_Played_Per_Game, 
                                               Assist_Per_Game=Assist_Per_Game))
                    
                    # return as character string that can be easily rendered
                    return(as.character(round(PointsPerGame, 2)))
                  }
                  
                    
                    
                    
                    preds(fit= fit, 
                          Free_Throw_Percentage = input$Free_Throw_Percentage,
                          Field_Goal_Percentage = input$Field_Goal_Percentage,
                          Minutes_Played_Per_Game = input$Minutes_Played_Per_Game,
                          Assist_Per_Game = input$Assist_Per_Game)
                  })
                }


# Run the application 
      shinyApp(ui = ui, server = server)
