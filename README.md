# ST558-FinalProject

This app allows the user to interact with credit risk data from the UCI Machine Learning Repository. The users can view the data, subset the data, download the data, build plots from the data, model the data, and make predictions based on the models without ever programming any code. The purpose of the app is to highlight some of the data science process and allow non statisticians to explore the data and model it. 

### List of Packages Needed to Run the App
- shiny
- tidyverse
- readxl
- ggcorrplot
- plotly
- tree
- caret
- randomForest
- DT

### A Line of Code That Would Install All of the Packages

install.packages( c("shiny", "tidyverse", "readxl", "ggcorrplot", "plotly", "tree", "caret", "randomForest", "DT") )

### runGithub Code to Run the App

shiny::runGitHub("Vito-Frank-Leonardo/ST558-FinalProject", ref = "main")
