library(shiny)
library(DT)
library(tidyverse)


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
    # reset table --------
    reset_table = tibble(role = rep(NA,15),
                         ratio = rep(0, 15),
                         ratio_s = rep(0, 15))
    
    
    observeEvent(input$reset,{
        values$df <- reset_table
        values$df_gen <- reset_table
    })
    
    
    # editable tables -------
    values <- reactiveValues()
    
    # ICU
    observe({
        values$df <- team_icu
    })

    proxy = dataTableProxy('x1')

    observeEvent(input$x1_cell_edit, {
        info = input$x1_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value

        values$df[i, j] <- isolate(coerceValue(v, values$df[i, j]))
        replaceData(proxy, values$df, resetPaging = FALSE)

    })

    
    
    # Non-ICU
    observe({
        values$df_gen <- team_gen
    })
    
    proxy_gen = dataTableProxy('x2')
    
    observeEvent(input$x2_cell_edit, {
        info = input$x2_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        
        values$df_gen[i, j] <- isolate(DT::coerceValue(v, values$df_gen[i, j]))
        replaceData(proxy_gen, values$df_gen, resetPaging = FALSE)  # important
        
    })
    

    
    # reference table display ---------
    output$x1 <- renderDT(
        values$df %>%
            rename(
                "Ratio" = ratio,
                "Ratio*" = ratio_s,
                Role = role
            ) ,
        caption = 'ICU',
        selection = 'none',
        editable = TRUE,
        server = TRUE,
        options = list(dom = "t", 
                       autoWidth = TRUE,
                       columnDefs = list(list(width = '150px', targets = "_all")),
                       pageLength = 15),
    )
    
    
    
    output$x2 <- renderDT(
        values$df_gen %>%
            rename(
                # "Bed to Person Ratio" = ratio,
                "Ratio" = ratio,
                "Ratio*" = ratio_s,
                Role = role
            ),
        caption = 'Non-ICU',
        selection = 'none',
        editable = TRUE,
        server = TRUE,
        options = list(dom = "t", 
                       autoWidth = TRUE,
                       columnDefs = list(list(width = '150px', targets = "_all")),
                       pageLength = 15
        )
    )
    
    # buttons --------
    
    observeEvent(input$update_gen, {
        updateTabsetPanel(session, "inTabset",selected = "Assumptions (i.e. staff ratios)")
    })
    
    observeEvent(input$calculate, {
        updateTabsetPanel(session, "inTabset",selected = "Normal (Tier 1)")
    })
    
    
    
    
    #  calculate staff needs---------

    icu_staff <- reactive({
        values$df %>% 
            filter(!is.na(role)) %>% 
            transmute(team_type = "icu",
                      role,
                      n_staff = ceiling(input$n_pt_icu/as.numeric(ratio)),
                      n_staff_strech = ceiling(input$n_pt_icu/as.numeric(ratio_s))) %>%
            mutate_if(is.numeric, as.integer)  
    })
    
    
    
    non_icu_staff <- reactive({
        values$df_gen %>% 
            filter(!is.na(role)) %>% 
            transmute(team_type = "gen",
                      role,
                      n_staff = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio)),
                      n_staff_strech = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio_s))) %>% 
            mutate_if(is.numeric, as.integer) 
    })
    
    # Display ------

    # normal mode
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
    
    
    output$table_normal <- renderTable(norm_staff_table() )
    
    # crisis mode
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
    


    
})
