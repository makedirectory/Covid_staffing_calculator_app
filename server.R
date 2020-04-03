library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    team_ratio = readRDS("./data/team_ratio.rds") %>% 
        mutate_if(is.numeric, as.integer)
    
    #interactive -----
    team_type = readRDS("./data/team_ratio.rds") %>% distinct(team_tpye) %>% pull

    # ICU ratio
    team_icu = readRDS("./data/team_ratio.rds") %>%
        filter(team_tpye == "ICU") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)

    team_role = team_icu  %>%
        distinct(role)

    values <- reactiveValues()
    values$df <- team_icu
    newEntry <- observe({
        if(input$update > 0) {
            newLine <- isolate(tibble(role = input$role, ratio = input$ratio, ratio_s = input$ratio_s))
            isolate(values$df <- full_join(values$df, newLine, by = "role") %>%
                        mutate(ratio = ifelse(!is.na(ratio.y), ratio.y, ratio.x),
                               ratio_s = ifelse(!is.na(ratio_s.y), ratio_s.y, ratio_s.x)) %>%
                        select(role, ratio, ratio_s))
        }
    })
    
    # non-ICU ratio ------
    # gen_ratio = team_ratio %>% 
    #     filter(team_tpye == "General") 
    
    team_gen = readRDS("./data/team_ratio.rds") %>%
        filter(team_tpye == "General") %>%
        transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch)
    
    team_role_gen = team_gen  %>%
        distinct(role)
    
    # values <- reactiveValues()
    values$df_gen <- team_gen
    newEntry_gen <- observe({
        if(input$update > 0) {
            newLine_gen <- isolate(tibble(role = input$role_gen, ratio = input$ratio_gen, ratio_s = input$ratio_s_gen))
            isolate(values$df_gen <- full_join(values$df_gen, newLine_gen, by = "role") %>%
                        mutate(ratio = ifelse(!is.na(ratio.y), ratio.y, ratio.x),
                               ratio_s = ifelse(!is.na(ratio_s.y), ratio_s.y, ratio_s.x)) %>%
                        select(role, ratio, ratio_s))
        }
    })
    
    
    
    
    # formula ---------
    icu_staff <- reactive({
         values$df %>% 
            transmute(role,
                      n_staff = ceiling(input$n_pt_icu/ratio),
                      n_staff_strech = ceiling(input$n_pt_icu/ratio_s)) %>%
            mutate_if(is.numeric, as.integer)  
    })
    
    non_icu_staff <- reactive({
        n_non_icu_pt = input$n_covid_pt - input$n_pt_icu
        
        values$df_gen %>% 
            transmute(role,
                      n_staff = ceiling(n_non_icu_pt/ratio),
                      n_staff_strech = ceiling(n_non_icu_pt/ratio_s)) %>% 
            mutate_if(is.numeric, as.integer) 
    })
    
    # Table of selected dataset ----
    output$table_icu <- renderTable({
        icu_staff() %>%
            rename("Staff Demand" = n_staff,
                   "Staff Demand (Crisis Mode)" = n_staff_strech,
                   Role = role)
    })
    
    output$table_gen <- renderTable({
        non_icu_staff() %>% 
            rename("Staff Demand" = n_staff,
                   "Staff Demand (Crisis Mode)" = n_staff_strech,
                   Role = role)
    })
    
    output$table_combine <- renderTable({
        staff_icu <- icu_staff()
        staff_gen <- non_icu_staff()
        
        full_join(staff_icu, staff_gen, by = "role") %>% 
            mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
            tidyext::row_sums(n_staff.x, n_staff.y, varname = "n_normal", na_rm = T) %>% 
            tidyext::row_sums(n_staff_strech.x, n_staff_strech.y, varname = "n_strech", na_rm = T) %>% 
            transmute(Role = role, "Staff Demand" = n_normal, "Staff Demand (Crisis Mode)" = n_strech) %>% 
            mutate_if(is.numeric, as.integer)  
            
    })
    
    # output$icu_ratio <- renderTable({
    #     icu_ratio %>% 
    #         select(-team_structure_id) %>% 
    #         rename("Team Type" = team_tpye,
    #                "Bed to Person Ratio" = n_bed_per_person,
    #                "Bed to Person Ratio (Crisis Mode)" = n_bed_per_person_stretch,
    #                Role = role
    #                )
    # })
    
    output$icu_ratio <- renderTable({
        values$df %>%
            rename(Role = role) %>% 
            mutate_if(is.numeric, as.integer)  
    })
    
    # output$gen_ratio <- renderTable({
    #     gen_ratio %>%
    #         select(-team_structure_id) %>%
    #         mutate(team_tpye = "Non-ICU") %>%
    #         rename("Team Type" = team_tpye,
    #                "Bed to Person Ratio" = n_bed_per_person,
    #                "Bed to Person Ratio (Crisis Mode)" = n_bed_per_person_stretch,
    #                Role = role
    #         )
    # })
    
    output$gen_ratio <- renderTable({
        values$df_gen %>%
            rename(Role = role) %>% 
            mutate_if(is.numeric, as.integer)  
    })
    
    
    
    

})
