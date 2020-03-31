#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    team_ratio = readRDS("./data/team_ratio.rds") %>% 
        mutate_if(is.numeric, as.integer)
    
    icu_ratio = team_ratio %>%
        filter(team_tpye == "ICU")
    
    gen_ratio = team_ratio %>% 
        filter(team_tpye == "General") 
    
    # formula ---------
    table_icu <- reactive({
        n_icu_pt = input$n_covid_pt*input$icu_ratio
        n_icu_pt_vent = ceiling(input$n_icu_pt*input$icu_vent_ratio)

        staff_icu = icu_ratio %>% 
            transmute(role,
                      n_staff = ceiling(n_icu_pt/n_bed_per_person),
                      n_staff_strech = ceiling(n_icu_pt/n_bed_per_person_stretch)) %>% 
            mutate_if(is.numeric, as.integer)
    
    })
    
    table_gen <- reactive({
        n_non_icu_pt = input$n_covid_pt*input$covid_general_ratio
        
        staff_gen = gen_ratio %>% 
            transmute(role,
                      n_staff = ceiling(n_non_icu_pt/n_bed_per_person),
                      n_staff_strech = ceiling(n_non_icu_pt/n_bed_per_person_stretch)) %>% 
            mutate_if(is.numeric, as.integer)
        
    })
    
    
    table_icu_test <- reactive({
        n_icu_pt = input$n_covid_pt*input$icu_ratio
        n_icu_pt_vent = ceiling(input$n_icu_pt*input$icu_vent_ratio)
        
        staff_icu = icu_ratio %>% 
            transmute(role,
                      n_staff = ceiling(n_icu_pt/n_bed_per_person),
                      n_staff_strech = ceiling(n_icu_pt/n_bed_per_person_stretch)) %>% 
            mutate_if(is.numeric, as.integer)
        
    })
    
    # Table of selected dataset ----
    output$table_icu <- renderTable({
        table_icu()
    })
    
    output$table_gen <- renderTable({
        table_gen()
    })
    
    output$table_combine <- renderTable({
        table_icu <- table_icu()
        table_gen <- table_gen()
        
        full_join(table_icu, table_gen, by = "role") %>% 
            mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
            tidyext::row_sums(n_staff.x, n_staff.x, varname = "n_normal", na_rm = T) %>% 
            tidyext::row_sums(n_staff_strech.x, n_staff_strech.y, varname = "n_strech", na_rm = T) %>% 
            transmute(Role = role, "Staff Demand" = n_normal, "Staff Demand Stretch" = n_strech) %>% 
            mutate_if(is.numeric, as.integer)  
            
    })
    
    output$icu_ratio <- renderTable({
        icu_ratio %>% 
            select(-team_structure_id)
    })
    
    output$gen_ratio <- renderTable({
        gen_ratio %>% 
            select(-team_structure_id)
    })
    

})
