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
library(DT)

# read team ratio -----------
team_ratio = readRDS("./data/team_ratio.rds") %>% 
    mutate_if(is.numeric, as.integer)

team_type = readRDS("./data/team_ratio.rds") %>% distinct(team_tpye) %>% pull

# ICU ratio
team_icu = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "ICU") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)

team_role = team_icu  %>%
    distinct(role)

# non-icu
team_gen = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "General") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)

team_role_gen = team_gen  %>%
    distinct(role)


# Define UI ---------
shinyUI(fluidPage(
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
    
    h4("Censes"),
    # input n covid pts
    numericInput("n_covid_pt", "Total COVID positive inpatients", 100, min = 0, max = 10000, step = 10,),
    # ICU n pt
    numericInput("n_pt_icu", "COVID positive inpatients requiring ICU-level care", 30, min = 0, max = 10000, step = 10,),
    
    numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10,),
    
    
   
    hr(),
    helpText(paste0("‘ICU-level bed’ includes any patient requiring an ICU bed or ICU-equivalent bed",
    " (i.e. non-ICU bed converted to ICU-level for COVID response)")
)
    
        ),
    
    
    mainPanel(
        fluidRow(
        tabsetPanel(
            tabPanel("Total Inpatient",
                     br(),
                     "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                     br(),
                     "Multiply as needed to account for shift changes.",
                     br(),
                     br(),
                     
                     # Output: Header + table of distribution ----
                     # h4("Total"),
                     tableOutput("table_combine"),
                     
                     column(8,
                            verbatimTextOutput("text"),
                            br(),
                            p("* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic..
                              Stretch ratios are based on currently available projections.
                              
                              ** A table of staffing ratios used for these calculations can be found at Reference Table tab"),
                            
            )
        ),
        tabPanel("ICU", 
                 # h4("ICU"),
                 tableOutput("table_icu")),
        
        tabPanel("Non-ICU",
                 # h4("Non-ICU"),
                 tableOutput("table_gen")

        # tabPanel("Assumptions (i.e. staff ratios)", 
        #          h4("ICU"),
        #          DTOutput("x1", width = "50%"),
        #          
        #          h4("Non-ICU"),
        #          DTOutput("x2", width = "50%")

        )
           
        )
        ),
        
        hr(),
        
        fluidRow(
            # h4("ICU"),
            column(5, DTOutput("x1")),
            
            # h4("Non-ICU"),
            column(5, DTOutput("x2"))
        )
    )
)
))
