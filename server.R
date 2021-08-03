# librarying packages
library(shiny)
library(tidyverse)
library(readxl)
library(ggcorrplot)
library(plotly)
library(tree)
library(caret)
library(randomForest)
library(DT)

# Importing Dataset
Credit <- read_excel(path = "default of credit card clients.xls",
                     col_names = TRUE, 
                     skip=1) %>% 
    rename(DEFAULT = `default payment next month`) %>%
    mutate(DEFAULT = factor(DEFAULT, levels = c(1,0), labels = c("Yes", "No")),
           SEX = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
           EDUCATION = factor(EDUCATION, levels = c(1,2,3,4), 
                              labels = c('Graduate School', 'University', 'High School', 'others')),
           MARRIAGE = factor(MARRIAGE, levels = c(1,2,3), 
                             labels = c('Married', 'Single', 'Others'))) %>%  select(-ID) %>% drop_na()
Credit.Numeric <- Credit %>% select(c(1,5,6:23))


# Define server logic required to create project
shinyServer(function(input, output, session) {
    
    # Reactive Table for Data Section
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
    
    # Output Table for Data Section
    output$data <- renderDataTable({
        datatable(tableData(),
                      options = list(scrollX = TRUE))
    })
    
    # Output Data for the Data Download Button
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("CreditDefaultSubset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableData(), file)
        })
    
    # Create a numbers represented columns of dataset based on user input
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
    
    
    # Create a plot based on user input
    output$plot <- renderPlotly({
        if(input$Type == 1){
            # interactive correlation plot
            ggplotly( ggcorrplot( cor(Credit %>% select(CorrPlotVar()) ), 
                                  hc.order = TRUE, type = "lower", outline.col = "white"))
            
        } else if(input$Type == 2){
            # interactive bar plot
            ggplotly(ggplot(data = Credit, 
                            mapping = aes_string(x = input$varPlot2X, 
                                                 y = input$varPlot2Y, 
                                                 fill = input$varPlot2X))+
                         geom_boxplot() + 
                         labs(title = paste0("Boxplot of ",
                                             input$varPlot2X," vs ",
                                             input$varPlot2Y )))
            
        } else{
            # 3d scatter plot
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
    
    # Create a table of numeric summaries based on user input
    SumTable <- reactive({
        if(input$Sum == 1){
            Credit.Numeric %>% 
                select(input$varSumm) %>%
                apply(MARGIN= 2,FUN = summary) %>%
                round(input$Digits) 
        } else data.frame(table(Credit[[which.max(names(Credit)==input$varTab1)]], 
                                Credit[[which.max(names(Credit)==input$varTab2)]]))
    })
    
    # Output table of numeric summaries
    output$summary <- renderDataTable({
        datatable(SumTable(), options = list(scrollX = TRUE))
    })
    
    # Output the fit results based on user input
    FitResults <- eventReactive(input$fit, {
        p <- input$train
        Train <- sample(1:nrow(Credit), p*nrow(Credit))
        Credit.Train <- Credit[Train,]
        Credit.Test <- Credit[-Train,]
        
        ### GLM
        glm.fit <- glm(DEFAULT ~ ., 
                       Credit.Train %>% select(input$LogistPred, DEFAULT), 
                       family = binomial)
        if(input$step== 'Yes'){
            glm.fit <- MASS::stepAIC(glm.fit, direction = "both", trace = FALSE)
        }
        glm.sum <- summary(glm.fit)
        threshold <- input$threshold
        glm.prob.train <- predict(glm.fit, type = "response", newdata = Credit.Train)
        glm.pred.train <- ifelse(glm.prob.train>threshold, 1, 0)
        glm.pred.train <- factor(glm.pred.train, levels=c(1,0), labels = c("Yes","No"))
        glm.trainMiss <- sum(glm.pred.train==Credit.Train$DEFAULT, na.rm=T)/length(Credit.Train$DEFAULT)
        
        glm.prob.test <- predict(glm.fit, type = "response", newdata = Credit.Test)
        glm.pred.test <- ifelse(glm.prob.test>threshold, 1, 0)
        glm.pred.test <- factor(glm.pred.test, levels=c(1,0), labels = c("Yes","No"))
        glm.testMiss <- sum(glm.pred.test==Credit.Test$DEFAULT, na.rm=T)/length(Credit.Test$DEFAULT)
        
        ### CART
        treeFit <- tree(DEFAULT~.,
                        data=select(Credit.Train,input$CARTpred, DEFAULT))
        if(input$Prune == 'Yes'){
            pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
            best <- pruneFit$size[which.min(pruneFit$dev)]
            treeFit <- prune.misclass(treeFit, best = best)
        }
        treeFit.summ <- summary(treeFit)
        treeFit.Miss.train <- treeFit.summ$misclass[1]/treeFit.summ$misclass[2]
        treeFit.pred.test <- predict(treeFit, newdata = Credit.Test %>% 
                                         select(-DEFAULT), type = "class")
        treeFit.Miss.test <- 1- 
            sum(treeFit.pred.test==Credit.Test$DEFAULT, na.rm=T)/length(Credit.Test$DEFAULT)
        
        ### RF
        
        if(input$CV == 'Yes'){
            k <- input$k
            mtry <- 1:input$Maxmtry
            tuning <- data.frame(mtry = mtry)
            rfFit <- train(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT), 
                           method ="rf",trControl = trainControl(method="cv", number=k), 
                           tuneGrid = tuning)
            rfFit <- rfFit$finalModel
        } else { 
            rfFit <- randomForest(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT),
                                  mtry = input$M, ntree = 200, importance = TRUE)
        }
        rf.trainMiss <- mean(rfFit$err.rate[,1])
        rfPred <- predict(rfFit, newdata = Credit.Test %>% select(input$RFpred,DEFAULT))
        rf.testMiss <- 1- sum(rfPred==Credit.Test$DEFAULT, na.rm=T)/
            length(Credit.Test$DEFAULT)
        
        data.frame(Logistic = round(c(glm.trainMiss, glm.testMiss),4),
                   DecisionTree = round(c(treeFit.Miss.train, treeFit.Miss.test),4),
                   RandomForest = round(c(rf.trainMiss, rf.testMiss),4),
                   row.names = c("Training Misclassification Rate", "Test Misclassification Rate"))
        
    })
    
    # Output fit statistics from model fits
    output$Fits <- renderDataTable({
        FitResults()
    })
    
    # Get logistic regression summary from user input
    glm.sum <- eventReactive(input$fit, {
        p <- input$train
        Train <- sample(1:nrow(Credit), p*nrow(Credit))
        Credit.Train <- Credit[Train,]
        Credit.Test <- Credit[-Train,]
        glm.fit <- glm(DEFAULT ~ ., 
                       Credit.Train %>% select(input$LogistPred, DEFAULT), 
                       family = binomial)
        if(input$step== 'Yes'){
            glm.fit <- MASS::stepAIC(glm.fit, direction = "both", trace = FALSE)
        }
        glm.sum <- summary(glm.fit)
    })
    
    # Output logistic regression summary
    output$Logistic <- renderPrint({
        print(glm.sum())
    })
    
    # Get classification tree summary from user input
    treeFit <- eventReactive(input$fit, {
        p <- input$train
        Train <- sample(1:nrow(Credit), p*nrow(Credit))
        Credit.Train <- Credit[Train,]
        Credit.Test <- Credit[-Train,]
        treeFit <- tree(DEFAULT~.,
                        data=select(Credit.Train,input$CARTpred, DEFAULT))
        if(input$Prune == 'Yes'){
            pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
            best <- pruneFit$size[which.min(pruneFit$dev)]
            treeFit <- prune.misclass(treeFit, best = best)
        }
        treeFit.summ <- summary(treeFit)
    })
    
    # output classification tree summary
    output$CART <- renderPrint({
        print(treeFit())
    })
    
    # Get random forest summary from user input
    rfFit <- eventReactive(input$fit, {
        p <- input$train
        Train <- sample(1:nrow(Credit), p*nrow(Credit))
        Credit.Train <- Credit[Train,]
        Credit.Test <- Credit[-Train,]
        if(input$CV == 'Yes'){
            k <- input$k
            mtry <- 1:input$Maxmtry
            tuning <- data.frame(mtry = mtry)
            rfFit <- train(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT), 
                           method ="rf",trControl = trainControl(method="cv", number=k), 
                           tuneGrid = tuning)
            rfFit <- rfFit$finalModel
        } else { 
            rfFit <- randomForest(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT),
                                  mtry = input$M, ntree = 200, importance = TRUE)
        }
    })
    
    # output random forest summary
    output$RF <- renderPrint({
        print(rfFit())
    })
    
    # Get prediction based on user input
    Prediction <- eventReactive(input$Predict,{
        p <- input$train
        Train <- sample(1:nrow(Credit), p*nrow(Credit))
        Credit.Train <- Credit[Train,]
        Credit.Test <- Credit[-Train,]
        
        ### GLM
        glm.fit <- glm(DEFAULT ~ ., 
                       Credit.Train %>% select(input$LogistPred, DEFAULT), 
                       family = binomial)
        if(input$step== 'Yes'){
            glm.fit <- MASS::stepAIC(glm.fit, direction = "both", trace = FALSE)
        }

        threshold <- input$threshold

        
        ### CART
        treeFit <- tree(DEFAULT~.,
                        data=select(Credit.Train,input$CARTpred, DEFAULT))
        if(input$Prune == 'Yes'){
            pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
            best <- pruneFit$size[which.min(pruneFit$dev)]
            treeFit <- prune.misclass(treeFit, best = best)
        }
        treeFit.summ <- summary(treeFit)
        treeFit.Miss.train <- treeFit.summ$misclass[1]/treeFit.summ$misclass[2]
        treeFit.pred.test <- predict(treeFit, newdata = Credit.Test %>% 
                                         select(-DEFAULT), type = "class")
        treeFit.Miss.test <- 1- 
            sum(treeFit.pred.test==Credit.Test$DEFAULT, na.rm=T)/length(Credit.Test$DEFAULT)
        
        ### RF
        
        if(input$CV == 'Yes'){
            k <- input$k
            mtry <- 1:input$Maxmtry
            tuning <- data.frame(mtry = mtry)
            rfFit <- train(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT), 
                           method ="rf",trControl = trainControl(method="cv", number=k), 
                           tuneGrid = tuning)
            rfFit <- rfFit$finalModel
        } else { 
            rfFit <- randomForest(DEFAULT ~ ., data = Credit.Train %>% select(input$RFpred,DEFAULT),
                                  mtry = input$M, ntree = 200, importance = TRUE)
        }
        rf.trainMiss <- mean(rfFit$err.rate[,1])
        rfPred <- predict(rfFit, newdata = Credit.Test %>% select(input$RFpred,DEFAULT))
        rf.testMiss <- 1- sum(rfPred==Credit.Test$DEFAULT, na.rm=T)/
            length(Credit.Test$DEFAULT)
        
        NewData <- data.frame(LIMIT_BAL = input$LIMIT_BAL,
                              SEX = factor(input$SEX),
                              EDUCATION = factor(input$EDUCATION), 
                              MARRIAGE = factor(input$MARRIAGE),
                              AGE = input$AGE ,
                              PAY_0 = as.numeric(input$PAY_0) ,
                              PAY_2 = as.numeric(input$PAY_2) ,
                              PAY_3 = as.numeric(input$PAY_3) ,
                              PAY_4 = as.numeric(input$PAY_4) ,
                              PAY_5 = as.numeric(input$PAY_5),
                              PAY_6 = as.numeric(input$PAY_6), 
                              BILL_AMT1= input$BILL_AMT1,
                              BILL_AMT2= input$BILL_AMT2,
                              BILL_AMT3= input$BILL_AMT3,
                              BILL_AMT4= input$BILL_AMT4,
                              BILL_AMT5= input$BILL_AMT5,
                              BILL_AMT6= input$BILL_AMT6,
                              PAY_AMT1= input$PAY_AMT1,
                              PAY_AMT2= input$PAY_AMT2,
                              PAY_AMT3= input$PAY_AMT3, 
                              PAY_AMT4= input$PAY_AMT4,
                              PAY_AMT5= input$PAY_AMT5,
                              PAY_AMT6= input$PAY_AMT6,
                              DEFAULT= 0)

        if(input$model == 1) {
            glm.prob.test <- predict(glm.fit, type = "response", newdata = NewData)
            glm.pred.test <- ifelse(glm.prob.test>threshold, 1, 0)
            data.frame(Default = factor(glm.pred.test, levels=c(1,0), labels = c("Yes","No")))
        } else if(input$model == 2){
            data.frame(Default = treeFit.pred.test <- predict(treeFit, newdata = NewData, type = "class"))
        } else{
            data.frame(Default = predict(rfFit, newdata = NewData))
        }
    })
    
    # Output prediction
    output$predict <- renderTable({
        Prediction()
    })
    
    
})
