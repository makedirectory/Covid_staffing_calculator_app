library(rhandsontable)
library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)


# read team ratio -----------
team_ratio = readRDS("./data/team_ratio.rds") %>%
    mutate_if(is.numeric, as.integer)

# ICU ratio
team_icu = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "ICU") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)


team_icu_shift = team_icu %>%
  mutate(n_shift_per_person = c(3,3,4,5,5,4,4,7,7,7,7))  # from Kyla input

# non-icu
team_gen = readRDS("./data/team_ratio.rds") %>%
    filter(team_tpye == "General") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)

team_gen_shift = team_gen %>%
  mutate(n_shift_per_person = c(3,3,4,5,5,4,4,7,7))  # from Kyla input

team_shift = full_join(team_icu_shift, team_gen_shift, by = "role") %>%
  mutate(n_shift_per_person = ifelse(is.na(n_shift_per_person.x), n_shift_per_person.y, n_shift_per_person.x)) %>%
  select(role, n_shift_per_person)


# Define UI --------
shinyUI(
  fluidPage(
    includeCSS("styles.css"),
    titlePanel("Estimate Your Staffing Needs"),
    fluidRow(

    # Sidebar layout with input and output definitions
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            # step1 census -------------
            h4(
                "Step 1 - Input your hospital’s total census (default values are examples)",
                class = "center-text margin-bottom10"
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
                width = "100%"
            ),

            # ICU n pt
            numericInput(
                "n_pt_icu",
                "COVID+ ICU-level Inpatients",
                60,
                min = 0,
                max = 1000,
                step = 10,
                width = "100%"
            ),

            # numericInput("n_pt_icu_vent", "ICU-level inpatients on ventilator", 20, min = 0, max = 10000, step = 10,),

            hr(),

            # step2 edit rato------------
            h4(
              "Step 2 - Add, modify or delete staff roles; update your hospital’s patient-to-staff ratios; and change the shift lengths and number of shifts per week to reflect your hospital’s staffing needs and workflow",
              class = "center-text margin-bottom10"
            ),

            actionButton(
              "update_gen",
              "Update Staffing",
              width = "100%",
              icon("user-md"),
              class = "main-button margin-top10"
            ),

            hr(),

            # step3 calcuate -----
            h4(
                "Step 3 - Calculate staffing needs",
                class = "center-text margin-bottom10"
            ),
            br(),

            actionButton(
              "calculate",
              "Calculate Results",
              width = "100%",
              icon("calculator"),
              class = "main-button margin-top10 margin-bottom20"
            ),

            helpText(
                p(
                    "*ICU-level inpatient’ includes any patient requiring an ICU bed or ICU-equivalent bed",
                    "(i.e. non-ICU bed converted to ICU-staffing level for COVID response)",
                    class = "center-text margin-bottom10"
                )
            )

        ),

        # main tables display ------------
        mainPanel(fluidRow(

            tags$style(HTML("
              .tabbable > .nav > li[class=active]    > a[data-value='Normal (Tier 1)'] {background-color: #9dc183; color:black}
              .tabbable > .nav > li[class=active]    > a[data-value='Crisis (Tier 2)'] {background-color: #8D021F; color:white}
            ")),

            tabsetPanel(
                id = "inTabset",

                # normal mode ---------
                tabPanel(
                  value = "Normal (Tier 1)",
                    "Normal Mode",
                    # br(),
                    # "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                    # br(),
                    # "Multiply as needed to account for shift changes.",
                    # br(),
                    # br(),

                  # buttons

                  br(),

                  p(strong("Select tables to show:")),
                  # buttons

                  shinyWidgets::materialSwitch(inputId="normal_any", label = "Any Given Time", value = TRUE, status = "success"),
                  shinyWidgets::materialSwitch(inputId="normal_day", label = "Daily Staffing Needs", value = FALSE, status = "success"),
                  shinyWidgets::materialSwitch(inputId="normal_week", label = "Weekly Staffing Needs", value = FALSE, status = "success"),

                  # table
                  hr(),
                  conditionalPanel(h4("Staff needs at any given time"),condition = "input.normal_any", div(tableOutput("table_normal"), class = "font-size")),
                  hr(),
                  conditionalPanel(h4("Daily Staff Needs (Accounting for shift hrs)"),condition = "input.normal_day", div(tableOutput("normal_day_table"), class = "font-size")),
                  hr(),
                  conditionalPanel(h4("Weekly Staff Needs (Accounting for shift hrs and number of shifts)"),condition = "input.normal_week", div(tableOutput("normal_week_table"), class = "font-size")),

                  p(  class = "margin-top10 text-margin",
                    "* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.
                        Crisis mode ratios are based on currently available projections."
                  ),

                    # downloadButton("downloadData_norm", "Download the Tables Above",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                    #
                ),

                # crisis mode ----

                tabPanel(
                    value = "Crisis (Tier 2)",
                    "Crisis Mode",
                    # br(),
                    # "Disclaimer: Staffing projections refer to institutional staff needs at any given point in time.",
                    # br(),
                    # "Multiply as needed to account for shift changes.",

                    p(strong("Select tables to show:"), class = "margin-top10"),
                    # buttons

                    shinyWidgets::materialSwitch(inputId="crisis_any", label = "Any Given Time", value = TRUE, status = "success"),
                    shinyWidgets::materialSwitch(inputId="crisis_day", label = "Daily Staffing Needs", value = FALSE, status = "success"),
                    shinyWidgets::materialSwitch(inputId="crisis_week", label = "Weekly Staffing Needs", value = FALSE, status = "success"),

                    hr(),
                    conditionalPanel(h4("Staff Needs at any given time"), condition = "input.crisis_any", div(tableOutput("table_crisis"), class = "font-size")),
                    hr(),
                    conditionalPanel(h4("Daily Staff Needs (Accounting for shift hrs)"), condition = "input.crisis_day", div(tableOutput("crisis_day_table"), class = "font-size")),
                    hr(),
                    conditionalPanel(h4("Weekly Staff Needs (Accounting for shift hrs and number of shifts)"), condition = "input.crisis_week", div(tableOutput("crisis_week_table"), class = "font-size")),

                    p( class = "margin-top20 text-margin",
                        "* Staffing estimates are based on actual staff-to-patient ratios used in ICU and non-ICU settings at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.
                              Crisis mode ratios are based on currently available projections."
                    ),

                    # downloadButton("downloadData_crisis", "Download the Tables Above",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
                ),

                # editable ratio --------
                tabPanel(
                    "Patient-to-Staff Ratios",

                    helpText(strong("Important note:"), class = "margin-top20 text-margin",
                             "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."),

                    # shinyWidgets::setSliderColor("#404040", 1),
                    # sliderInput("reduction",label="Expected Staff Reduction (eg. sick)", min = 0, max = 100, post  = " %", value = 30),
                    #

                    actionButton(
                      "reset",
                      "Clear Table",
                      icon("table"),
                      class = "main-button margin-bottom20 margin-top10"
                    ),
                    
                    actionButton(
                      "reset_to_ori",
                      "Reset to Default",
                      icon("undo"),
                      class = "main-button margin-bottom20 margin-top10"
                    ),
                    
                    p(strong("Right click"), "in a cell to add and delete row;", "select cell and type the new value",
                      style = "font-size:16px"),

                    h4("ICU"),

                    div(rHandsontableOutput("x1"), class = "font-size margin-bottom10"),

                    # downloadButton("downloadData_icu_ratio", "Download ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),


                    h4("Non-ICU"),

                    div(rHandsontableOutput("x2"), class = "font-size margin-bottom10"),

                    # downloadButton("downloadData_non_icu_ratio", "Download Non-ICU Staffing Ratio",
                    #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),

                    downloadButton(
                      "downloadData_all_ratio",
                      "Download Staffing Ratios",
                      width = "100%",
                      class = "main-button margin-bottom20 margin-top20"
                    ),

                    p( class = "margin-top20 text-margin",
                        strong("Role:"), " List of possible staff roles",
                        br(),
                        strong("Ratio (Normal)"), " = the patient:staff ratio (i.e. how many patients each staff member cares for)",
                        br(),
                        strong("Ratio (Crisis Mode)"), " = the patient:staff ratio during a ‘crisis mode’ (ie. the maximum number patients each staff member can care for)",
                        br(),
                        br(),
                        br(),
                        "* Default patient-to-staff ratios are based on real staffing ratios at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.",
                        br(),
                        br()
                    ),
                )
            ),
          ),
        )
    )
)))
