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
    
    # hr(),
    # h4("ICU Ratio"),
    # selectInput("role", "Role:", choices=unique(team_role), width = "400px"),
    # div(style="display:inline-block",
    #     numericInput("ratio", "Ratio", 2, min = 1, max = 100, width = "150px")),
    # div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
    # div(style="display:inline-block",
    #     numericInput("ratio_s", "Ratio (Crisis)", 4, min = 1, max = 100, width = "150px")),
    # actionButton("update_icu", "Update Ratio"),
    # 
    # hr(),
    # h4("Non-ICU Ratio"),
    # selectInput("role_gen", "Role:", choices=unique(team_role_gen), width = "400px"),
    # div(style="display:inline-block",
    #     numericInput("ratio_gen", "Ratio", 5, min = 1, max = 100, width = "150px")),
    # div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
    # div(style="display:inline-block",
    #     numericInput("ratio_s_gen", "Ratio (Crisis)", 10, min = 1, max = 100, width = "150px")),
    # actionButton("update_gen", "Update Ratio"),
    # 
    # # color input box
    # tags$style("#ratio_s_gen {background-color:#D3D3D3;}"),
    # tags$style("#ratio {background-color:#D3D3D3;}"),
    # tags$style("#ratio_gen {background-color:#D3D3D3;}"),
    # tags$style("#ratio_s {background-color:#D3D3D3;}"),
    
    
    hr(),
    helpText(paste0("‘ICU-level bed’ includes any patient requiring an ICU bed or ICU-equivalent bed",
    " (i.e. non-ICU bed converted to ICU-level for COVID response)")
)
    
        ),
    
    
    mainPanel(
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
                 tableOutput("table_gen")),

        tabPanel("Assumptions (i.e. staff ratios)", 
                 h4("ICU"),
                 DTOutput("x1"),
                 
                 h4("Non-ICU"),
                 DTOutput("x2")

        )
           
        )
    )
)
))
