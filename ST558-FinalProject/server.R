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
library(ggcorrplot)
library(plotly)
library(webshot)
#install_phantomjs(force=TRUE)

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
    
    output$data <- renderDataTable({
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
    
    CorrPlotVar <- reactive({
            varlist2 <- as.numeric(input$varPlot1)
            if(6 %in% varlist2){
                varlist2 <- c(varlist2,7:11)
            }
            if(12 %in% varlist2){
                varlist2 <- c(varlist2,13:17)
            }
            if(18 %in% varlist2){
                varlist2 <- c(varlist2,19:23)
            }
            CorrPlotVar <- varlist2
    })
    
    
    
    output$plot <- renderPlotly({
        if(input$Type == 1){
            
            ggplotly( ggcorrplot( cor(Credit %>% select(CorrPlotVar()) ), 
                                  hc.order = TRUE, type = "lower", outline.col = "white"))
            
        } else if(input$Type == 2){
            
            ggplotly(ggplot(data = Credit, 
                            mapping = aes_string(x = input$varPlot2X, 
                                                 y = input$varPlot2Y, 
                                                 fill = input$varPlot2X))+
                         geom_boxplot() + 
                         labs(title = paste0("Boxplot of ",
                                             input$varPlot2X," vs ",
                                             input$varPlot2Y )))
            
        } else{
            plot_ly ( data = Credit,
                                  x = ~get(input$varPlot3X),
                                  y = ~get(input$varPlot3Y),
                                  z = ~get(input$varPlot3Z),
                                  color = ~Credit$DEFAULT, colors = c('red', 'blue'),
                                  type = 'scatter3d' ,
                                  mode = 'markers',
                                  marker = list(size = 2))
        }
    })   

})
