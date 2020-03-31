#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID Staffing Demand Calculator"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
    
    # input n covid pts
    numericInput("n_covid_pt", "Total COVID positive inpatients", 100, min = 0, max = 10000, step = 10),
    # ICU n pt
    numericInput("n_pt_icu", "COVID positive inpatients requiring ICU-level care", 30, min = 0, max = 10000, step = 10),
    
    numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10),
    
    helpText(paste0("* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings ",
    "at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic."), 
    helpText(" “Stretch” ratios are based on currently available projections.") ,
    helpText("** A table of staffing ratios used for these calculations can be found at Reference Table tab")
)
    
        ),
    
    
    mainPanel(
        tabsetPanel(
            tabPanel("Staff Table",
                     # Output: Header + table of distribution ----
                     h4("Total"),
                     tableOutput("table_combine"),
                     
                     h4("ICU"),
                     tableOutput("table_icu"),
                     
                     h4("Non-ICU"),
                     tableOutput("table_gen")
                     ),
            tabPanel("Reference Table", 
                     h4("ICU"),
                     tableOutput("icu_ratio"),
                     
                     h4("Non-ICU"),
                     tableOutput("gen_ratio")
                     )
            # tabPanel("Tab 3", "This panel is intentionally left blank")
        )
    )
)
))
