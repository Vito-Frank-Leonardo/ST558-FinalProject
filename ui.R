library(shiny)
library(markdown)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)

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
Credit.Numeric <- Credit %>% select(c(1,5,6:23))
Credit.Cat <- Credit %>% select(c(2:4,24))

# Define UI for application that draws a histogram
shinyUI(navbarPage("ST558 Final Project",
                   
                   
                   
############################ About #############################################                   
                   
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
                   
                   
############################# Data ################################################                   
                   
                   
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxInput("Subset", strong("Select a Subset?")),
                                
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
                                              choiceValues = c(1, 
                                                               2, 
                                                               3,
                                                               4,
                                                               5,
                                                               6,
                                                               12,
                                                               18
                                                               ),
                                              selected = c(1:24)
                                  )
                                ),
                                checkboxInput("Filter", strong("Filter Default?")),
                                
                                conditionalPanel(
                                  condition = "input.Filter",
                                  selectInput(inputId = "Def", label = "Default", 
                                              choices = c("Yes", "No"))
                                ),
                                
                                downloadButton('downloadData', 'Download data')
                              ),
                                
                                
                              mainPanel(
                                dataTableOutput("data")
                              )
                            )
                   ),
                   
                   
                   
############################ Data Exploration ################################################                   
                   
                   
                   tabPanel("Data Exploration",
                            sidebarLayout(
                            sidebarPanel(
                              
                              radioButtons(inputId = "Type", label = h2("Select the Plot Type"), 
                                           choiceNames = c("Correlation Plot of Numeric Predictors",
                                                           "Boxplot",
                                                           "3d Scatterplot"),
                                           choiceValues = c(1,2,3)),
                              
                              conditionalPanel(
                                condition = "input.Type == '1'",
                                checkboxGroupInput(inputId = "varPlot1", 
                                                   label = "Correlation Plot Variables", 
                                                   choiceNames = c("Credit Limit",
                                                                 "Age",
                                                                 "Past Payment Status",
                                                                 "Past Payment Amount",
                                                                 "Bill Statement History"
                                                  ),
                                                  choiceValues = c(1, 
                                                                  5,
                                                                  6,
                                                                  12,
                                                                  18
                                                  ),
                                                  selected = c(1,5,6:23)
                                                  )
                              ),
                              
                              conditionalPanel(
                                condition = "input.Type == '2'",
                                radioButtons(inputId = "varPlot2X", 
                                                   label = "X Axis", 
                                                   choiceNames = c("Default",
                                                                   "Sex",
                                                                   "Education",
                                                                   "Marriage"
                                                                   ),
                                                   choiceValues = c("DEFAULT", 
                                                                    "SEX",
                                                                    "EDUCATION",
                                                                    "MARRIAGE"
                                                   ),
                                                   selected = "DEFAULT"
                                ),
                                radioButtons(inputId = "varPlot2Y", 
                                                   label = "Y Axis", 
                                                   choiceNames = c("Credit Limit",
                                                                   "Age",
                                                                   "Payment History",
                                                                   "Bill Amount",
                                                                   "Payment Amount"
                                                   ),
                                                   choiceValues = c("LIMIT_BAL", 
                                                                    "AGE",
                                                                    "PAY_0",
                                                                    "BILL_AMT1",
                                                                    "PAY_AMT1"
                                                   ),
                                                   selected = c("LIMIT_BAL")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.Type == '3'",
                                selectInput(inputId = "varPlot3X", 
                                                   label = "Scatterplot Variable X", 
                                                   choices = names(Credit)
                                                   ,
                                                   selected = "DEFAULT"
                                ),
                                selectInput(inputId = "varPlot3Y", 
                                            label = "Scatterplot Variable Y", 
                                            choices = names(Credit)
                                            ,
                                            selected = "LIMIT_BAL"
                                ),
                                selectInput(inputId = "varPlot3Z", 
                                            label = "Scatterplot Variable Z", 
                                            choices = names(Credit)
                                            ,
                                            selected = "AGE"
                                )
                              ),
                              
                              downloadButton('downloadPlot', 'Download Plot'),
                              
                              radioButtons(inputId = "Sum", label = h2("Select the Summary Type"), 
                                           choiceNames = c("Quantitative Summary",
                                                           "Contingency Table"),
                                           choiceValues = c(1,2)),
                              
                              conditionalPanel(
                                condition = "input.Sum == '1'",
                                numericInput(inputId = "Digits", 
                                             label = "Select the number of digits for rounding",
                                             value = 2)
                                ,
                                selectizeInput(inputId = "varSumm", 
                                            label = "Variable to Summarize", 
                                            choices = names(Credit.Numeric),
                                            multiple = TRUE,
                                            selected = names(Credit.Numeric))
                                ),
                              
                                conditionalPanel(
                                condition = "input.Sum == '2'",
                                selectInput(inputId = "varTab1", 
                                            label = "Variable 1 to Tabulate", 
                                            choices = names(Credit.Cat),
                                            selected = "DEFAULT"
                                ),
                                selectInput(inputId = "varTab2", 
                                            label = "Variable 2 to Tabulate", 
                                            choices = names(Credit.Cat)
                                            ,
                                            selected = "SEX")
                              )
                              
                            ),
                            
                            mainPanel(
                              plotlyOutput("plot"),
                              dataTableOutput("summary"),
                              tags$script('
                  document.getElementById("downloadPlot").onclick = function() {
                  var plot = $("#selectplot").val();
                  if(plot == "plot1"){
                    var gd = document.getElementById("plot");
                  }else{
                    var gd = document.getElementById("plot");
                  }
                  Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                    var a = window.document.createElement("a");
                    a.href = url; 
                    a.type = "image/png";
                    a.download = "plot.png";
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);                      
                  });
                  }
                  ')
                            )
                   )
                   ),
                   
                   
                   
##############################  Modeling ###########################################                   
                   
                   navbarMenu("Modeling",
                              tabPanel("Modeling Info",
                                       h2("Logistic Regression"),
                                       "Logistic Regressions is a popular general"
                                       
                                       
                                       
                                       
                                       
                                       ),
                              
                              
                              
                              tabPanel("Model Fitting",
                                       sidebarLayout(
                                         sidebarPanel(
                                          numericInput("train", 
                                                       label = "Proportion of Data In Training Set",
                                                       value = 0.8, min = 0.1, max = .99 ),
                                          h2("Logistic Regression"),
                                          selectizeInput(inputId = "LogistPred", 
                                                         label = "Predictors for Logistic Model", 
                                                         choices = names(Credit %>% select(-DEFAULT)),
                                                         multiple = TRUE,
                                                         selected = names(Credit %>% select(-DEFAULT))),
                                          numericInput("threshold",
                                                       "Threshold Probability For Classification",
                                                       value = 0.5, min = 0.1, max = .99),
                                          selectInput(inputId = "step",
                                                      label = "Use Stepwise Predictor Selection After Fitting?",
                                                      choices = c("Yes", "No"),
                                                      selected = "No"),
                                          h2("Decision Tree"), 
                                          selectizeInput(inputId = "CARTpred", 
                                                         label = "Predictors for Decision Tree", 
                                                         choices = names(Credit %>% select(-DEFAULT)),
                                                         multiple = TRUE,
                                                         selected = names(Credit %>% select(-DEFAULT))),
                                          selectInput(inputId = "Prune",
                                                      label = "Prune the Tree After Fitting?",
                                                      choices = c("Yes", "No"),
                                                      selected = "No"),
                                          h2("Random Forest"),
                                          selectizeInput(inputId = "RFpred", 
                                                         label = "Predictors for Random Forest", 
                                                         choices = names(Credit %>% select(-DEFAULT)),
                                                         multiple = TRUE,
                                                         selected = names(Credit %>% select(-DEFAULT))),
                                          selectInput(inputId = "CV",
                                                      label = "Use K-fold CV to find best m?",
                                                      choices = c("Yes", "No"),
                                                      selected = "No"),
                                          conditionalPanel(condition = "input.CV == 'Yes'",
                                                           numericInput(inputId = "Maxmtry",
                                                                        label = "Choose Max Mtry",
                                                                        value = 8, min = 2, max = 15),
                                                           numericInput(inputId = "k",
                                                                        label = "K?",
                                                                        value = 5, min = 2, max = 10)),
                                          conditionalPanel(condition = "input.CV == 'No'",
                                                           numericInput(inputId = "M",
                                                                        label = "Choose M",
                                                                        value = 5, min = 2, max = 15))
                                          ,
                                          actionButton("fit", "Fit The Models!")
                                          
                                         ),
                                        mainPanel(
                                          h2("Fit Statistics"),
                                          dataTableOutput("Fits"),
                                          h2("Logistic Regression Summary Output"),
                                          verbatimTextOutput("Logistic"),
                                          h2("Decision Tree Summary Output"),
                                          verbatimTextOutput("CART"),
                                          h2("Random Forest Summary Output"),
                                          verbatimTextOutput("RF")
                                        )
                                       )),
                              tabPanel("Prediction",
                                       sidebarLayout(
                                         sidebarPanel(
                                         radioButtons(inputId = "model",
                                                      label = "Choose Your Model!",
                                                    choiceNames = c("Logistic Regression",
                                                                    "Decision Tree",
                                                                    "Random Forest"),
                                                    choiceValues = c(1,2,3)),
                                       h2("Choose Your Predictor Inputs!"),
                                       numericInput("LIMIT_BAL",
                                                    label = "LIMIT_BAL",
                                                    value = mean(Credit$LIMIT_BAL), 
                                                    min = min(Credit$LIMIT_BAL), 
                                                    max = min(Credit$LIMIT_BAL)),
                                       selectInput(inputId = "SEX",
                                                   label = "SEX",
                                                   choices = unique(Credit$SEX)),
                                       selectInput(inputId = "EDUCATION",
                                                   label = "EDUCATION",
                                                   choices = unique(Credit$EDUCATION)),
                                       selectInput(inputId = "MARRIAGE",
                                                   label = "MARRIAGE",
                                                   choices = unique(Credit$MARRIAGE)),
                                       numericInput("AGE",
                                                    label = "AGE",
                                                    value = mean(Credit$AGE), 
                                                    min = min(Credit$AGE), 
                                                    max = min(Credit$AGE)),
                                       selectInput(inputId = "PAY_0",
                                                   label = "PAY_0",
                                                   choices = unique(Credit$PAY_0)),
                                       selectInput(inputId = "PAY_2",
                                                   label = "PAY_2",
                                                   choices = unique(Credit$PAY_2)),
                                       selectInput(inputId = "PAY_3",
                                                   label = "PAY_3",
                                                   choices = unique(Credit$PAY_3)),
                                       selectInput(inputId = "PAY_4",
                                                   label = "PAY_4",
                                                   choices = unique(Credit$PAY_4)),
                                       selectInput(inputId = "PAY_5",
                                                   label = "PAY_5",
                                                   choices = unique(Credit$PAY_5)),
                                       selectInput(inputId = "PAY_6",
                                                   label = "PAY_6",
                                                   choices = unique(Credit$PAY_6)),
                                       numericInput("BILL_AMT1",
                                                    label = "BILL_AMT1",
                                                    value = mean(Credit$BILL_AMT1), 
                                                    min = min(Credit$BILL_AMT1), 
                                                    max = min(Credit$BILL_AMT1)),
                                       numericInput("BILL_AMT2",
                                                    label = "BILL_AMT2",
                                                    value = mean(Credit$BILL_AMT2), 
                                                    min = min(Credit$BILL_AMT2), 
                                                    max = min(Credit$BILL_AMT2)),
                                       numericInput("BILL_AMT3",
                                                    label = "BILL_AMT3",
                                                    value = mean(Credit$BILL_AMT3), 
                                                    min = min(Credit$BILL_AMT3), 
                                                    max = min(Credit$BILL_AMT3)),
                                       numericInput("BILL_AMT4",
                                                    label = "BILL_AMT4",
                                                    value = mean(Credit$BILL_AMT4), 
                                                    min = min(Credit$BILL_AMT4), 
                                                    max = min(Credit$BILL_AMT4)),
                                       numericInput("BILL_AMT5",
                                                    label = "BILL_AMT5",
                                                    value = mean(Credit$BILL_AMT5), 
                                                    min = min(Credit$BILL_AMT5), 
                                                    max = min(Credit$BILL_AMT5)),
                                       numericInput("BILL_AMT6",
                                                    label = "BILL_AMT6",
                                                    value = mean(Credit$BILL_AMT6), 
                                                    min = min(Credit$BILL_AMT6), 
                                                    max = min(Credit$BILL_AMT6)),
                                       numericInput("PAY_AMT1",
                                                    label = "PAY_AMT1",
                                                    value = mean(Credit$PAY_AMT1), 
                                                    min = min(Credit$PAY_AMT1), 
                                                    max = min(Credit$PAY_AMT1)),
                                       numericInput("PAY_AMT2",
                                                    label = "PAY_AMT2",
                                                    value = mean(Credit$PAY_AMT2), 
                                                    min = min(Credit$PAY_AMT2), 
                                                    max = min(Credit$PAY_AMT2)),
                                       numericInput("PAY_AMT3",
                                                    label = "PAY_AMT3",
                                                    value = mean(Credit$PAY_AMT3), 
                                                    min = min(Credit$PAY_AMT3), 
                                                    max = min(Credit$PAY_AMT3)),
                                       numericInput("PAY_AMT4",
                                                    label = "PAY_AMT4",
                                                    value = mean(Credit$PAY_AMT4), 
                                                    min = min(Credit$PAY_AMT4), 
                                                    max = min(Credit$PAY_AMT4)),
                                       numericInput("PAY_AMT5",
                                                    label = "PAY_AMT5",
                                                    value = mean(Credit$PAY_AMT5), 
                                                    min = min(Credit$PAY_AMT5), 
                                                    max = min(Credit$PAY_AMT5)),
                                       numericInput("PAY_AMT6",
                                                    label = "PAY_AMT6",
                                                    value = mean(Credit$PAY_AMT6), 
                                                    min = min(Credit$PAY_AMT6), 
                                                    max = min(Credit$PAY_AMT6)),
                                      
                                       actionButton("Predict", "Predict!")
                                       
                                         ),
                                       
                                       mainPanel(
                                         h2("Prediction!"),
                                         tableOutput("predict"))
                              ))
)))
