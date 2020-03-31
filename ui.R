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
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
    
    # input n covid pts
    numericInput("n_covid_pt", "Total COVID positive inpatients", 100, min = 0, max = 10000, step = 10),
    # ICU n pt
    numericInput("n_pt_icu", "COVID positive inpatients requiring ICU-level care", 30, min = 0, max = 10000, step = 10),
    
    numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10),
    
    helpText(paste0("‘ICU-level bed’ includes any patient requiring an ICU bed or ICU-equivalent bed",
    " (i.e. non-ICU bed converted to ICU-level for COVID response)") 
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
                     tableOutput("table_gen"),
                     
                     column(8,
                            verbatimTextOutput("text"),
                            br(),
                            p("* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic..
                              Stretch ratios are based on current available projections.
                              
                              ** A table of staffing ratios used for these calculations can be found at Reference Table tab")
            )
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
