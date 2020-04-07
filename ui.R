
library(rhandsontable)
library(shiny)
library(tidyverse)
library(DT)

# read team ratio -----------
team_ratio = readRDS("./data/team_ratio.rds") %>%
    mutate_if(is.numeric, as.integer)

# ICU ratio
team_icu = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "ICU") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)

# non-icu
team_gen = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "General") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)



# Define UI --------
shinyUI(fluidPage(fluidRow(
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            # step1 census -------------
            h4(
                "Step 1 - Input your hospital’s total census (default values are examples)"
            ),
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
                180,
                min = 0,
                max = 1000,
                step = 10,
                width = "70%"
            ),
            
            # ICU n pt
            numericInput(
                "n_pt_icu",
                "COVID+ ICU-level Inpatients",
                60,
                min = 0,
                max = 1000,
                step = 10,
                width = "50%"
            ),
            
            # numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10,),
            
            hr(),
            
            # step2 edit rato------------
            h4(
                "Step 2 - Update your hospital’s patient-to-staff ratios and add, modify, or delete staff roles to reflect your hospital’s specific staffing needs."
            ),
            br(),
            
            actionButton("update_gen", "Update Staffing", icon("user-md"),
                         style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
            
            
            hr(),
            
            # step3 calcuate -----
            h4(
                "Step 3 - Calculate Staffing Needs"
            ),
            br(),
            
            actionButton("calculate", "Calculate Results", icon("calculator"),
                         style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
            
            
            
            br(),
            br(),
            hr(),
            helpText(
                paste0(
                    "*ICU-level inpatient’ includes any patient requiring an ICU bed or ICU-equivalent bed",
                    "(i.e. non-ICU bed converted to ICU-staffing level for COVID response)"
                )
            )
            
        ),
        
        # main tables display ------------
        mainPanel(fluidRow(
            
            h3("Estimate Your Staffing Needs"),
            
            tags$style(HTML("
        .tabbable > .nav > li[class=active]    > a[data-value='Normal (Tier 1)'] {background-color: #9dc183; color:black}
        .tabbable > .nav > li[class=active]    > a[data-value='Crisis (Tier 2)'] {background-color: #8D021F; color:white}
  ")),
            
            tabsetPanel(
                id = "inTabset",
                
                # normal mode ---------
                tabPanel(
                    "Normal (Tier 1)",
                    br(),
                    "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                    br(),
                    "Multiply as needed to account for shift changes.",
                    br(),
                    br(),
                    
                    # table
                    div(tableOutput("table_normal"), style = "font-size:120%"),
                    
                    
                    column(
                        8,
                        verbatimTextOutput("text"),
                        br(),
                        p(
                            "* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic..
                              Crisis mode ratios are based on currently available projections"
                        )
                        
                    ),
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    downloadButton("downloadData_norm", "Download Table Above",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                    
                ),
                
                # crisis mode ----
                
                tabPanel(
                    "Crisis (Tier 2)",
                    br(),
                    "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                    br(),
                    "Multiply as needed to account for shift changes.",
                    br(),
                    br(),
                    
                    div(tableOutput("table_crisis"), style = "font-size:120%"),

                    column(
                        8,
                        verbatimTextOutput("text3"),
                        br(),
                        p(
                            "* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.
                              Crisis mode ratios are based on currently available projections."
                        )
                        
                    ),
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    downloadButton("downloadData_crisis", "Download Table Above",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                ),
                
                # editable ratio --------
                tabPanel(
                    "Patient-to-Staff Ratios",
                    br(),
                    
                    helpText(strong("Important note:"), 
                             "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."),
                    
            
                    actionButton("reset", "Clear Table", icon("table"),
                                 style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    actionButton("reset_to_ori", "Reset to Default", icon("undo"),
                                 style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    

                    br(),
                    br(),
                    
                    p(strong("Right click"), "in a cell to add and delete row;", "select cell and type the new value",
                      style = "font-size:16px"),

                    h4("ICU"),
                    
                    div(rHandsontableOutput("x1"), style = "font-size: 120%"),
                    
                    br(), 
                    
                    # downloadButton("downloadData_icu_ratio", "Download ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    h4("Non-ICU"),
                    
                    div(rHandsontableOutput("x2"), style = "font-size: 120%"),
                    
                    br(),
                    
                    
                    # downloadButton("downloadData_non_icu_ratio", "Download Non-ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    downloadButton("downloadData_all_ratio", "Download Staffing Ratios",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    column(
                        12,
                        br(),
                        tags$div(
                            "Role: List of possible staff roles",
                            tags$br(),
                            "Ratio (normal) = the patient:staff ratio (i.e. how many patients each staff member cares for)",
                            tags$br(),
                            "Ratio (Crisis Mode) = the patient:staff ratio during a ‘crisis mode’ (ie. the maximum number patients each staff member can care for)",
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            "* Default patient-to-staff ratios are based on real staffing ratios at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.",
                            br(),
                            br()
                        )
                    ),
                    
                    

                )
                
            ),
            
        ),)
    )
)))
