

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


# Define UI --------
shinyUI(fluidPage(
    
    fluidRow(
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
    # step1-------------
    h4("Step 1 - Input your hospital’s patient censuses here (the default values are examples):"),
    # num input style 
    # tags$style("#n_covid_pt {font-size:20px;height:25px; color: gray;}"),
    # tags$style("#n_pt_icu {font-size:20px;height:25px; color: gray;}"),
    # tags$style("#n_pt_icu {color: red;
    #                              font-size: 20px;
    #                              font-style: italic;
    #                              }"),
    
    # input n covid pts
    numericInput(
        "n_covid_pt",
        "Total COVID+ Inpatients (ICU and Non-ICU)",
        1000,
        min = 0,
        max = 1000,
        step = 10,
        width = "70%"
    ),
    # ICU n pt
    numericInput(
        "n_pt_icu",
        "COVID+ ICU Inpatients",
        300,
        min = 0,
        max = 1000,
        step = 10,
        width = "50%"
    ),
    
    # numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10,),
    
    hr(),
    
    # step2------------
    h4(
        "Step 2- Update your hospital’s patient:staff ratios and add, modify or delete staff roles to reflect your hospital’s specific staff organization."
    ),
    br(),
    
    actionButton("update_gen", "Update Staffing", icon("user-md"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),


    br(),
    br(),
   
    
   
    hr(),
    helpText(paste0("‘ICU-level bed’ includes any patient requiring an ICU bed or ICU-equivalent bed",
    " (i.e. non-ICU bed converted to ICU-level for COVID response)")
)
    
        ),
    
    
    mainPanel(
        fluidRow(
            
            h3("View your total, ICU, and non-ICU staffing estimates in the table below: "),
            
           
        tabsetPanel(
            id="inTabset",
            
            tabPanel("Normal (Tier 1)",
                     br(),
                     "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                     br(),
                     "Multiply as needed to account for shift changes.",
                     br(),
                     br(),
                     
                     # Output: Header + table of distribution ----
                     # h4("Total"),
                     div(tableOutput("table_normal"), style = "font-size:120%"),
                     
                     column(8,
                            verbatimTextOutput("text"),
                            br(),
                            p("* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic..
                              Crisis mode ratios are based on currently available projections"),
                            
            ),
            
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            downloadButton("downloadData_norm", "Download Normal Staffing", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            
        ),
        
        tabPanel("Crisis (Tier 2)",
                 br(),
                 "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                 br(),
                 "Multiply as needed to account for shift changes.",
                 br(),
                 br(),
                 
                 div(tableOutput("table_crisis"), style = "font-size:120%"),
                 
                 
                 column(8,
                        verbatimTextOutput("text3"),
                        br(),
                        p("* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic..
                              Crisis mode ratios are based on currently available projections"),
                        
                 ),
                 
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 downloadButton("downloadData_crisis", "Download Crisis Staffing", 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 
            
                 
                 ),
        
        
        tabPanel("Assumptions (i.e. staff ratios)", 
                 br(),
                 actionButton("calculate", "Calculate Results", icon("calculator"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 
                 actionButton("reset", "Your Own Role and Ratio"),

                 br(),
               
                     column(div(dataTableOutput ("x1"), style = "font-size: 120%"),
                     width = 5),
                 
                 
                 rHandsontableOutput("x1"),

                     column(div(dataTableOutput ("x2"), style = "font-size: 120%"),
                            width = 5, offset = 1),

                 column(12, br(),
                 tags$div(
                     "Role: List of possible staff roles", tags$br(),
                     "Ratio  = the patient:staff ratio (i.e. how many patients each staff member cares for)", tags$br(),
                     "Ratio* = the patient:staff ratio during a ‘crisis mode’ (ie. the maximum number patients each staff member can care for)")
                 )
                
        )
           
        ),
     
        ),
        

    )
)
)
        # fluidRow(
        #     
        #     hr(),
        #     
        #     br(),
        #     
        # 
        #     column(div(dataTableOutput ("x1"), style = "font-size: 120%"),
        #     width = 4, offset = 2),
        #     
        #     column(div(dataTableOutput ("x2"), style = "font-size: 120%"),
        #            width = 4)
        # )
))
