#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Murder clusters in the United States"),
  
  # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        sliderInput("years",
                    "Years",
                    min = 1976,
                    max = 2015,
                    value = c(2006,2015),
                    sep="")
     ),
     
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("murdermap"),
      p(),
      dataTableOutput('table')
      
    )
  )
)
 )
