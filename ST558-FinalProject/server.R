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
            varlist1 <- input$var1
            if("Status" %in% varlist1){
                varlist1[[which.max("Status" == varlist1 )]] <- 6:11
            }
            if("Amount" %in% varlist1){
                varlist1[[which.max("Amount" == varlist1 )]] <- 18:23
            }
            if("Bill" %in% varlist1){
                varlist1[[which.max("Bill" == varlist1 )]] <- 12:17
            }
            varlist1 <- c(as.numeric(unlist(varlist1)),24)
            tableData <- Credit %>% select(varlist1)
        } else tableData <- Credit
        if(input$Def){
            tableData <- tableData %>% filter(DEFAULT %in% input$Def) 
        } 
    })
    
    output$table <- renderDataTable({
        datatable(tableData(),
                      options = list(scrollX = TRUE))
    })

 
    
    
    
    
    

})
