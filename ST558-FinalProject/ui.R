#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(tidyverse)
library(readxl)
library(DT)

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

# Define UI for application that draws a histogram
shinyUI(navbarPage("ST558 Final Project",
                   
                   
                   
                   
                   
                   tabPanel("About",
                            fluidRow(
                                column(6,
                                       includeMarkdown("about.md")
                                ),
                                column(3,
                                       img(src="creditriskcollage.jpg",
                                           width = 510,
                                           height =510)
                                       )
                                )
                            ),
                   
                   
                   
                   
                   
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxInput("Subset", "Select a Subset?"),
                                
                                conditionalPanel(
                                  condition = "input.Subset",
                                  checkboxGroupInput(inputId = "var1", label = "Variables", 
                                              choiceNames = c("Credit Limit", 
                                                          "Gender", 
                                                          "Education",
                                                          "Marriage",
                                                          "Age",
                                                          "Past Payment Status",
                                                          "Past Payment Amount",
                                                          "Bill Statement History"
                                                          ),
                                              choiceValues = list(1, 
                                                               2, 
                                                               3,
                                                               4,
                                                               5,
                                                               'Status',
                                                               'Amount',
                                                               'Bill'
                                                               ),
                                              selected = c("Credit Limit", 
                                                           "Gender", 
                                                           "Education",
                                                           "Marriage",
                                                           "Age",
                                                           "Past Payment Status",
                                                           "Past Payment Amount",
                                                           "Bill Statement History"
                                              )
                                  )
                                ),
                                checkboxInput("Filter", "Filter Data?"),
                                
                                conditionalPanel(
                                  condition = "input.Filter",
                                  selectInput(inputId = "Def", label = "Default", 
                                              choices = c("Yes", "No"))
                                )
                              ),
                                
                                
                              mainPanel(
                                dataTableOutput("table")
                              )
                            )
                   ),
                   
                   
                   
                   
                   
                   
                   tabPanel("Data Exploration"),
                   navbarMenu("Modeling",
                              tabPanel("Modeling Info"),
                              tabPanel("Model Fitting"),
                              tabPanel("Prediction"))
))
