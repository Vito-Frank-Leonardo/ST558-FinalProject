library(shiny)
library(tidyverse)
library(readxl)
library(ggcorrplot)
library(plotly)
library(tree)
library(caret)
library(randomForest)
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
                             labels = c('Married', 'Single', 'Others'))) %>%  select(-ID) %>% drop_na()
Credit.Numeric <- Credit %>% select(c(1,5,6:23))


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
    
    
    SumTable <- reactive({
        if(input$Sum == 1){
            Credit.Numeric %>% 
                select(input$varSumm) %>%
                apply(MARGIN= 2,FUN = summary) %>%
                round(input$Digits) 
        } else data.frame(table(Credit[[which.max(names(Credit)==input$varTab1)]], 
                                Credit[[which.max(names(Credit)==input$varTab2)]]))
    })
    output$summary <- renderDataTable({
        datatable(SumTable(), options = list(scrollX = TRUE))
    })
    
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
        if(input$prune == "Yes"){
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
    
    output$Fits <- renderDataTable({
        FitResults()
    })
    
    glm.sum <- eventReactive(input$fit, {
        glm.fit <- glm(DEFAULT ~ ., 
                       Credit.Train %>% select(input$LogistPred, DEFAULT), 
                       family = binomial)
        if(input$step== 'Yes'){
            glm.fit <- MASS::stepAIC(glm.fit, direction = "both", trace = FALSE)
        }
        glm.sum <- summary(glm.fit)
    })
    
    output$Logistic <- renderPrint({
        print(glm.sum())
    })
    
    treeFit <- eventReactive(input$fit, {
        treeFit <- tree(DEFAULT~.,
                        data=select(Credit.Train,input$CARTpred, DEFAULT))
        if(input$Prune == 'Yes'){
            pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
            best <- pruneFit$size[which.min(pruneFit$dev)]
            treeFit <- prune.misclass(treeFit, best = best)
        }
        treeFit.summ <- summary(treeFit)
    })
    output$CART <- renderPrint({
        print(treeFit())
    })
    
    
    rfFit <- eventReactive(input$fit, {
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
    
    output$RF <- renderPrint({
        print(rfFit())
    })
    
})
