library(shiny)
library(DT)
library(tidyverse)
library(rhandsontable)
library(shinyWidgets)


source("functions/calculator_staff_needs.R")
source("functions/display_by_mode.R")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # read team ratio -----------
    team_ratio = readxl::read_xlsx("./data/team_ratio_shift.xlsx") %>%
        mutate_if(is.numeric, as.integer)

    # ICU
    team_icu = team_ratio %>%
        filter(team_type == "ICU") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                  shift_length_hr, shift_per_week)

    # non-icu
    team_gen = team_ratio %>%
        filter(team_type == "General") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                  shift_length_hr, shift_per_week)


  # interactive -------------------------------------

    # reference ratio --------

    # editable tables -------
    values <- reactiveValues()


    # reset reference table -------
    reset_table = tibble(role = c("Role1", NA, NA),
                         ratio = as.numeric(rep(0, 3)),
                         ratio_s = as.numeric(rep(0, 3)),
                         shift_length_hr = rep(0,3),
                         shift_per_week = rep(0,3))


    observeEvent(input$reset,{
        output$x1 <- renderRHandsontable({
            rhandsontable(
                reset_table %>%
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Stretch)" = ratio_s,
                        Role = role,
                        "Shift Length(hours)" = shift_length_hr,
                        "Number of Shifts/week" = shift_per_week
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
            ) %>%
                hot_cols(colWidths = 100)
        })

        output$x2 <- renderRHandsontable({
            rhandsontable(
                reset_table %>%
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Stretch)" = ratio_s,
                        Role = role,
                        "Shift Length(hours)" = shift_length_hr,
                        "Number of Shifts/week" = shift_per_week
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
            ) %>%
                hot_cols(colWidths = 100)
        })
    })

    # reset to default
    observeEvent(input$reset_to_ori,{
        output$x1 <- renderRHandsontable({
            rhandsontable(
                team_icu %>%
                  rename(
                    "Ratio (Normal)" = ratio,
                    "Ratio (Stretch)" = ratio_s,
                    Role = role,
                    "Shift Length(hours)" = shift_length_hr,
                    "Number of Shifts/week" = shift_per_week
                  ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
            ) %>%
                hot_cols(colWidths = 100)
        })

        output$x2 <- renderRHandsontable({
            rhandsontable(
                team_gen %>%
                  rename(
                    "Ratio (Normal)" = ratio,
                    "Ratio (Stretch)" = ratio_s,
                    Role = role,
                    "Shift Length(hours)" = shift_length_hr,
                    "Number of Shifts/week" = shift_per_week
                  ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
            ) %>%
                hot_cols(colWidths = 100)
        })
    })




    # reference table (editable) ---------
    output$x1 <- renderRHandsontable({
        rhandsontable(
            team_icu %>%
              rename(
                "Ratio (Normal)" = ratio,
                "Ratio (Stretch)" = ratio_s,
                Role = role,
                "Shift Length(hours)" = shift_length_hr,
                "Number of Shifts/week" = shift_per_week
              ) %>%
                mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
        ) %>%
            hot_cols(colWidths = 100)

    })

    output$x2 <- renderRHandsontable({
        rhandsontable(
            team_gen %>%
              rename(
                "Ratio (Normal)" = ratio,
                "Ratio (Stretch)" = ratio_s,
                Role = role,
                "Shift Length(hours)" = shift_length_hr,
                "Number of Shifts/week" = shift_per_week
              ) %>%
                mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
        ) %>%
            hot_cols(colWidths = 100)

        })



    # buttons --------

    observeEvent(input$update_gen, {
        updateTabsetPanel(session, "inTabset",selected = "Patient-to-Staff Ratios")
    })

    observeEvent(input$calculate, {
        updateTabsetPanel(session, "inTabset",selected = "Normal (Tier 1)")
    })



    #  calculate staff needs---------

    icu_staff <- reactive({

        # if null
        if(is.null(input$x1)) return(calculator_staff_needs("icu",team_icu, input$n_pt_icu) %>%
                                       mutate_if(is.numeric, as.integer))


        # if edit
        values$df = hot_to_r(input$x1) %>%
            # rename back
        rename(
            ratio = "Ratio (Normal)",
            ratio_s = "Ratio (Stretch)",
            role = Role,
            shift_length_hr = "Shift Length(hours)",
            shift_per_week = "Number of Shifts/week"
        ) %>%
            filter(!is.na(role)) %>%
            calculator_staff_needs(team_type = "icu",n_pt = input$n_pt_icu) %>%
            mutate_if(is.numeric, as.integer)

    })



    non_icu_staff <- reactive({

      values$non_icu = input$n_covid_pt - input$n_pt_icu

        # if null
        if(is.null(input$x2)) return(calculator_staff_needs("gen", team_gen, values$non_icu))

        # if edit

        values$df_gen = hot_to_r(input$x2) %>%
            # rename back ----
        rename(
            ratio = "Ratio (Normal)",
            ratio_s = "Ratio (Stretch)",
            role = Role,
            shift_length_hr = "Shift Length(hours)",
            shift_per_week = "Number of Shifts/week"
        ) %>%
            filter(!is.na(role)) %>%
            # calculator_staff_needs("gen", values$non_icu)
          transmute(team_type = "gen",
                    role,
                    n_staff = ceiling(values$non_icu/as.numeric(ratio)),
                    n_staff_strech = ceiling(values$non_icu/as.numeric(ratio_s)),
                    n_staff_day = n_staff*(24/shift_length_hr),
                    n_staff_strech_day = n_staff_strech*(24/shift_length_hr),
                    n_staff_week = n_staff_day*7/shift_per_week,
                    n_staff_strech_week = n_staff_day*7/shift_per_week) %>%
          mutate_if(is.numeric, as.integer)
    })




    # normal mode -----------

    # without shift
    norm_staff_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
         display_by_mode(quo(n_staff)) %>%
            rename(
                Role = role,
                "Non-ICU" = gen,
                "ICU" = icu,
                "All Inpatient" = all
            )
    })



    # needs per day
    norm_staff_day_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
        display_by_mode(quo(n_staff_day)) %>%
        rename(
          Role = role,
          "Non-ICU" = gen,
          "ICU" = icu,
          "All Inpatient" = all
        )
    })

    # needs per week
    norm_staff_week_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
        display_by_mode(quo(n_staff_week)) %>%
        # mutate(`Accounting for Staff Reduction` = as.integer(all* (1+input$reduction/100))) %>%
        rename(
          Role = role,
          "Non-ICU" = gen,
          "ICU" = icu,
          "All Inpatient" = all
        )
    })






    #  crisis mode -----------
    # no shift
    crisis_staff_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
        display_by_mode(quo(n_staff_strech)) %>%
        rename(
          Role = role,
          "Non-ICU" = gen,
          "ICU" = icu,
          "All Inpatient" = all
        )
    })

    # daily
    crisis_staff_day_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
        display_by_mode(quo(n_staff_strech_day)) %>%
        rename(
          Role = role,
          "Non-ICU" = gen,
          "ICU" = icu,
          "All Inpatient" = all
        )
    })

    # weekly
    crisis_staff_week_table <- reactive({
      rbind(non_icu_staff(), icu_staff()) %>%
        display_by_mode(quo(n_staff_strech_week)) %>%
        # mutate(`Accounting for Staff Reduction` = as.integer(all* (1+input$reduction/100))) %>%
        rename(
          Role = role,
          "Non-ICU" = gen,
          "ICU" = icu,
          "All Inpatient" = all
        )
    })







    # display demand tables-----
    # normal
    output$table_normal <- renderTable(norm_staff_table())

    output$normal_day_table <- renderTable(norm_staff_day_table())

    output$normal_week_table <- renderTable(norm_staff_week_table())

    # crisis
    output$table_crisis <- renderTable(crisis_staff_table())

    output$crisis_day_table <- renderTable(crisis_staff_day_table())

    output$crisis_week_table <- renderTable(crisis_staff_week_table())






   # dowload table -------

    data_crisis <- reactive({
      list(
        any_time = crisis_staff_table(),
        daily = crisis_staff_day_table(),
        weekly = crisis_staff_week_table()
      )
    })

    output$downloadData_crisis <- downloadHandler(
      filename = function() {"staffing_crisis.xlsx"},
      content = function(file) {
        writexl::write_xlsx(data_crisis(), path = file)
        }
    )


    output$downloadData_norm <- downloadHandler(
        filename = function() {
            paste('staffing_normal', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write.csv(norm_staff_table(), con)
        }
    )


    data_norm <- reactive({
      list(
        any_time = norm_staff_table(),
        daily = norm_staff_day_table(),
        weekly = norm_staff_week_table()
      )
    })

    output$downloadData_crisis <- downloadHandler(
      filename = function() {"staffing_norm.xlsx"},
      content = function(file) {
        writexl::write_xlsx(data_norm(), path = file)
      }
    )


    output$downloadData_icu_ratio <- downloadHandler(
        filename = function() {
            paste('ICU_Staffing_role_and_ratio', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            finalDF <- hot_to_r(input$x1)
            write.csv(finalDF, con)
        }
    )


    output$downloadData_non_icu_ratio <- downloadHandler(
        filename = function() {
            paste('Non_icu_Staffing_role_and_ratio', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            finalDF_non_icu <- hot_to_r(input$x2)
            write.csv(finalDF_non_icu, con)
        }
    )

    output$downloadData_all_ratio <- downloadHandler(
        filename = function() {
            paste('Staffing_role_and_ratio', Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            finalDF <- hot_to_r(input$x1) %>%
                mutate(team_type = "ICU") %>%
                rename(role = Role,
                       n_bed_per_person = "Ratio (Normal)" ,
                       n_bed_per_person_crisis = "Ratio (Stretch)") %>%
                select(team_type, everything())

            finalDF_non_icu <- hot_to_r(input$x2) %>%
                mutate(team_type = "General") %>%
                rename(role = Role,
                       n_bed_per_person = "Ratio (Normal)" ,
                       n_bed_per_person_crisis = "Ratio (Stretch)") %>%
                select(team_type, everything())


            all_ratio = rbind(finalDF, finalDF_non_icu)

            writexl::write_xlsx(all_ratio, path = con)
        }
    )



    # output$downloadData_ratio <- downloadHandler(
    #     filename = function() {
    #         paste('Staffing_role_and_ratio', Sys.Date(), '.csv', sep='')
    #     },
    #     content = function(con) {
    #         write.csv(norm_staff_table(), con)
    #     }
    # )







})
