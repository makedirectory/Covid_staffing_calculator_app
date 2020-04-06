
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
                "Step 1 - Input your hospital’s patient censuses here (the default values are examples):"
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
                "COVID+ ICU Inpatients",
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
                "Step 2- Update your hospital’s patient:staff ratios and add, modify or delete staff roles to reflect your hospital’s specific staff organization."
            ),
            br(),
            
            actionButton("update_gen", "Update Staffing", icon("user-md"),
                         style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
            
            
            br(),
            br(),
            hr(),
            helpText(
                paste0(
                    "‘ICU-level bed’ includes any patient requiring an ICU bed or ICU-equivalent bed",
                    " (i.e. non-ICU bed converted to ICU-level for COVID response)"
                )
            )
            
        ),
        
        # main tables display ------------
        mainPanel(fluidRow(
            h3("View your total, ICU, and non-ICU staffing estimates in the table below: "),
            
            
        # tab color ----    
  #           tags$style(HTML("
  #   .tabbable > .nav > li > a[data-value='Normal (Tier 1)'] {background-color: green;  color:black}
  #   .tabbable > .nav > li > a[data-value='Crisis (Tier 2)'] {background-color: red;   color:white}
  #   .tabbable > .nav > li > a[data-value='Patient to Staff Ratio'] {background-color: gray;  color:white}
  #   .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  # ")),
  
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
                    downloadButton("downloadData_norm", "Download Normal Staffing",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                    
                ),
                
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
                    downloadButton("downloadData_crisis", "Download Crisis Staffing",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                ),
                
                # editable ratio --------
                tabPanel(
                    "Patient to Staff Ratio",
                    br(),
                    
                    helpText(strong("Important note:"), 
                             "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."),
                    
            
                    
                    actionButton("calculate", "Calculate Results", icon("calculator"),
                                 style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    br(),
                    br(),
                    
                    actionButton("reset", "Clear Table", icon("table"),
                                 style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    actionButton("reset_to_ori", "Reset to Default", icon("undo"),
                                 style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    br(),
                    helpText(strong("Right click"), "in a cell to add and delete row.;", strong("Double click"), "in a cell to edit."),

                    h5("ICU"),
                    
                    div(rHandsontableOutput("x1"), style = "font-size: 120%"),
                    
                    br(), 
                    
                    # downloadButton("downloadData_icu_ratio", "Download ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    h5("Non-ICU"),
                    
                    div(rHandsontableOutput("x2"), style = "font-size: 120%"),
                    
                    br(),
                    
                    
                    # downloadButton("downloadData_non_icu_ratio", "Download Non-ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    downloadButton("downloadData_all_ratio", "Download All Staffing Ratio",
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                    
                    
                    column(
                        12,
                        br(),
                        tags$div(
                            "Role: List of possible staff roles",
                            tags$br(),
                            "Ratio (normal) = the patient:staff ratio (i.e. how many patients each staff member cares for)",
                            tags$br(),
                            "Ratio (Crisis Mode) = the patient:staff ratio during a ‘crisis mode’ (ie. the maximum number patients each staff member can care for)"
                        )
                    )

                )
                
            ),
            
        ),)
    )
)))
