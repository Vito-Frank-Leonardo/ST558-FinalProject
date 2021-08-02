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
library(readxl)

Credit <- read_excel(path = "default of credit card clients.xls",
                     col_names = TRUE, 
                     skip=1) %>% 
    rename(DEFAULT = `default payment next month`) %>%
    mutate(DEFAULT = factor(DEFAULT, levels = c(1,0), labels = c("Yes", "No")),
           SEX = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
           EDUCATION = factor(EDUCATION, levels = c(1,2,3,4), 
                              labels = c('Graduate School', 'University', 'High School', 'others')),
           MARRIAGE = factor(MARRIAGE, levels = c(1,2,3), 
                             labels = c('Married', 'Single', 'Others'))) %>%
    select(-ID) %>% drop_na()


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    tableData <- reactive({
        if(input$Subset){
            varlist1 <- as.numeric(input$var1)
            if(6 %in% varlist1){
                varlist1 <- c(varlist1,7:11)
            }
            if(12 %in% varlist1){
                varlist1 <- c(varlist1,13:17)
            }
            if(18 %in% varlist1){
                varlist1 <- c(varlist1,19:23)
            }
            tableData <- Credit %>% select(varlist1,24)
        } else tableData <- Credit
        if(input$Filter){
            tableData <- tableData %>% filter(DEFAULT %in% input$Def) 
        } else tableData <- tableData
    })
    
    output$table <- renderDataTable({
        datatable(tableData(),
                      options = list(scrollX = TRUE))
    })

    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("CreditDefaultSubset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableData(), file)
        })
    
    
    
    
    

})
