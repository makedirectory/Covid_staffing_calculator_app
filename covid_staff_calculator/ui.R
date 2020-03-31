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

    # Application title
    titlePanel("Covid Staff Calculator"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
    
    
    # input n covid pts
    numericInput("n_covid_pt", "Number of COVID Patient", 100, min = 0, max = 10000, step = 10),
    # ICU ratio
    numericInput("icu_ratio", "Ratio ICU Admission", 0.3, min = 0, max = 1, step = 0.1),
    numericInput("icu_vent_ratio", "Ratio of Vent Among ICU Admission", 0.2, min = 0, max = 1, step = 0.1),
    # general
    numericInput("covid_general_ratio", "Ratio of Non ICU admission", 0.2, min = 0, max = 1, step = 0.1)
        ),
    
    
    mainPanel(
        # Output: Header + table of distribution ----
        h4("ICU"),
        tableOutput("table_icu"),
        
        h4("General"),
        tableOutput("table_gen")
    )
)
))
