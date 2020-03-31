#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    team_ratio = readRDS("./data/team_ratio.rds")

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
    

})
