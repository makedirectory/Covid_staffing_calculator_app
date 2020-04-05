library(shiny)
library(DT)
library(tidyverse)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
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
    
    
    # interactive 
    
        # reference ratio --------
    values <- reactiveValues()
    
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
        
        # values$df = datatable(values$df)
        
        values$df[i, j] <- isolate(coerceValue(v, values$df[i, j]))
        replaceData(proxy, values$df, resetPaging = FALSE)  # important
        
        
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
        
        # values$df_gen = datatable(values$df_gen)
        
        values$df_gen[i, j] <- isolate(DT::coerceValue(v, values$df_gen[i, j]))
        replaceData(proxy_gen, values$df_gen, resetPaging = FALSE)  # important
        
    })
    

    
    # reference table display ---------
    output$x1 <- renderDT(
        values$df %>%
            rename(
                "P:S" = ratio,
                "P:S*" = ratio_s,
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
                "P:S" = ratio,
                "P:S*" = ratio_s,
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
    
    observeEvent(input$update_gen, {
        updateTabsetPanel(session, "inTabset",selected = "Assumptions (i.e. staff ratios)")
    })
    
    
    
    
    
    #  calculate staff needs---------
    icu_staff <- reactive({
         values$df %>% 
            transmute(role,
                      n_staff = ceiling(input$n_pt_icu/as.numeric(ratio)),
                      n_staff_strech = ceiling(input$n_pt_icu/as.numeric(ratio_s))) %>%
            mutate_if(is.numeric, as.integer)  
    })
    
    non_icu_staff <- reactive({
        values$df_gen %>% 
            transmute(role,
                      n_staff = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio)),
                      n_staff_strech = ceiling((input$n_covid_pt - input$n_pt_icu)/as.numeric(ratio_s))) %>% 
            mutate_if(is.numeric, as.integer) 
    })
    
    # Table of selected dataset ----

    #icu
    icu_staff_table = reactive({
        icu_staff() %>%
        rename("Staff Demand" = n_staff,
               "Staff Demand (Crisis Mode)" = n_staff_strech,
               Role = role) %>% 
        filter(Role != "")
    })
    
    output$table_icu <- renderTable(
        icu_staff_table()
    )
    
    #gen
    gen_staff_table = reactive({
            non_icu_staff() %>%
                rename("Staff Demand" = n_staff,
                       "Staff Demand (Crisis Mode)" = n_staff_strech,
                       Role = role) %>%
                filter(Role != "")
        })
    
    output$table_gen <- renderTable(
        gen_staff_table()
    )
    
    # combine
    combine_staff_table = reactive({
        staff_icu <- icu_staff()
        staff_gen <- non_icu_staff()
        
        full_join(staff_icu, staff_gen, by = "role") %>% 
            mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
            tidyext::row_sums(n_staff.x, n_staff.y, varname = "n_normal", na_rm = T) %>% 
            tidyext::row_sums(n_staff_strech.x, n_staff_strech.y, varname = "n_strech", na_rm = T) %>% 
            transmute(Role = role, "Staff Demand" = n_normal, "Staff Demand (Crisis Mode)" = n_strech) %>% 
            mutate_if(is.numeric, as.integer)  %>% 
            filter(Role != "")
    })
    
    output$table_combine <- renderTable({
        combine_staff_table()
    })
    
    
    
   # dowload table -------
    data_list <- reactive({
        list(
            Total_Inpatient = combine_staff_table(),
            ICU = icu_staff_table(),
            Non_ICU = gen_staff_table()
        )
    })

    
   output$downloadData <- downloadHandler(
       filename = function(){
           "Staffing_tables.xlsx"
       },
       content = function(file) {
           writexl::write_xlsx(data_list(), path = file)
       }
   )
    
 

    
    
})
