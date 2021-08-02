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


# Define UI for application that draws a histogram
shinyUI(navbarPage("ST558 Final Project",
                   tabPanel("About",
                            fluidRow(
                                column(6,
                                       includeMarkdown("about.md")
                                ),
                                column(3,
                                       img(src="creditriskcollage.jpg")
                                       )
                                )
                            ),
                   tabPanel("Data"),
                   tabPanel("Data Exploration"),
                   navbarMenu("Modeling",
                              tabPanel("Modeling Info"),
                              tabPanel("Model Fitting"),
                              tabPanel("Prediction"))
))
