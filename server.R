library(shiny)
library(DT)
library(tidyverse)
library(rhandsontable)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # read team ratio -----------
    team_ratio = readRDS("./data/team_ratio.rds") %>% 
        mutate_if(is.numeric, as.integer)
    
    # ICU 
    team_icu = readRDS("./data/team_ratio.rds") %>%
        filter(team_tpye == "ICU") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)
    
    # non-icu
    team_gen = readRDS("./data/team_ratio.rds") %>%
        filter(team_tpye == "General") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)
    

    
    # interactive --------------------------------------

    # reference ratio --------
    
    # editable tables -------
    values <- reactiveValues()
    
    # reset reference table -------
    reset_table = tibble(role = c("Role1", NA, NA),
                         ratio = as.numeric(rep(0, 3)),
                         ratio_s = as.numeric(rep(0, 3)))
    
    
    observeEvent(input$reset,{
        output$x1 <- renderRHandsontable({
            rhandsontable(
                reset_table %>%
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Crisis Mode)" = ratio_s,
                        Role = role
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
            ) %>% 
                hot_cols(colWidths = 100) 
        })
        
        output$x2 <- renderRHandsontable({
            rhandsontable(
                reset_table %>% 
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Crisis Mode)" = ratio_s,
                        Role = role
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
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
                        "Ratio (Crisis Mode)" = ratio_s,
                        Role = role
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
            ) %>% 
                hot_cols(colWidths = 100) 
        })
        
        output$x2 <- renderRHandsontable({
            rhandsontable(
                team_gen %>% 
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Crisis Mode)" = ratio_s,
                        Role = role
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
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
                    "Ratio (Crisis Mode)" = ratio_s,
                    Role = role
                ) %>%
                mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
        ) %>% 
            hot_cols(colWidths = 100) 
            
    })
    
    output$x2 <- renderRHandsontable({
        rhandsontable(
            team_gen %>% 
                rename(
                    "Ratio (Normal)" = ratio,
                    "Ratio (Crisis Mode)" = ratio_s,
                    Role = role
                ) %>%
                mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
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
        if(is.null(input$x1)) return(team_icu %>% 
                                         transmute(team_type = "icu",
                                                   role,
                                                   n_staff = ceiling(input$n_pt_icu/as.numeric(ratio)),
                                                   n_staff_strech = ceiling(input$n_pt_icu/as.numeric(ratio_s))) %>%
                                         mutate_if(is.numeric, as.integer))
        
        
        # if edit
        values$df = hot_to_r(input$x1) %>%
            # rename back ---- 
        rename(
            ratio = "Ratio (Normal)",
            ratio_s = "Ratio (Crisis Mode)",
            role = Role
        ) %>%
            
            filter(!is.na(role)) %>%
            transmute(team_type = "icu",
                      role,
                      n_staff = ceiling(input$n_pt_icu/as.numeric(ratio)),
                      n_staff_strech = ceiling(input$n_pt_icu/as.numeric(ratio_s))) %>%
            mutate_if(is.numeric, as.integer) 
    })
    
    non_icu_staff <- reactive({
        # if null
        if(is.null(input$x2)) return(team_gen %>% 
                                         transmute(team_type = "gen",
                                                   role,
                                                   n_staff = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio)),
                                                   n_staff_strech = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio_s))) %>% 
                                         mutate_if(is.numeric, as.integer) )
        # if edit
        
        values$df_gen = hot_to_r(input$x2) %>% 
            # rename back ---- 
        rename(
            ratio = "Ratio (Normal)",
            ratio_s = "Ratio (Crisis Mode)",
            role = Role
        ) %>%
            
            filter(!is.na(role)) %>% 
            transmute(team_type = "gen",
                      role,
                      n_staff = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio)),
                      n_staff_strech = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio_s))) %>% 
            mutate_if(is.numeric, as.integer) 
    })
    
    
    
    norm_staff_table <- reactive({
        rbind(non_icu_staff(), icu_staff()) %>% 
            filter(!is.na(role)) %>%
            select(team_type, role, n_staff) %>%
            pivot_wider(names_from = team_type,
                        values_from = n_staff) %>%
            tidyext::row_sums(gen, icu, varname = "all", na_rm = TRUE) %>%
            filter(all != 0) %>%
            rename(
                Role = role,
                "Non-ICU" = gen,
                "ICU" = icu,
                "All Inpatient" = all
            ) %>%
            mutate_if(is.numeric, as.integer)
    })
    
    
    #  crisis mode
    crisis_staff_table = reactive({
        rbind(non_icu_staff(), icu_staff()) %>%
            filter(!is.na(role)) %>%
            select(team_type, role, n_staff_strech) %>%
            pivot_wider(names_from = team_type,
                        values_from = n_staff_strech) %>%
            tidyext::row_sums(gen, icu, varname = "all", na_rm = TRUE) %>%
            filter(all != 0) %>%
            rename(
                Role = role,
                "Non-ICU" = gen,
                "ICU" = icu,
                "All Inpatient" = all
            ) %>%
            mutate_if(is.numeric, as.integer)
    })
   

    
    

    # display demand tables-----
    output$table_normal <- renderTable(norm_staff_table() )
    
    output$table_crisis <- renderTable(
        crisis_staff_table()
    ) 

    
    
    
   # dowload table -------
 
    output$downloadData_crisis <- downloadHandler(
      filename = function() {
        paste('staffing_crisis', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(crisis_staff_table(), con)
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
                       n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
                select(team_type, everything())
            
            finalDF_non_icu <- hot_to_r(input$x2) %>% 
                mutate(team_type = "General") %>% 
                rename(role = Role,
                       n_bed_per_person = "Ratio (Normal)" ,
                       n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
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
