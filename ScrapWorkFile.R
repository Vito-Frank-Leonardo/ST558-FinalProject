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

attach(Credit)
str(Credit)
dim(Credit)

##### Data 



##### Code for Data Visualization

#Numerical Summary 1 with categorical variables
table(DEFAULT, SEX)

# Plot 1
ggplot(data = Credit, mapping = aes(x = DEFAULT, y = LIMIT_BAL, fill = DEFAULT))+
  geom_boxplot()

  
"Payment History 0",
"Payment History 2",
"Payment History 3",
"Payment History 4",
"Payment History 5",
"Payment History 6",
"Bill Amount 1",
"Bill Amount 2",
"Bill Amount 3",
"Bill Amount 4",
"Bill Amount 5",
"Bill Amount 6",
"Payment Amount 1",
"Payment Amount 2",
"Payment Amount 3",
"Payment Amount 4",
"Payment Amount 5",
"Payment Amount 6"
),
choiceValues = c("LIMIT_BAL", 
                 "AGE",
                 "PAY_0",
                 "PAY_2",
                 "PAY_3",
                 "PAY_4",
                 "PAY_5",
                 "PAY_6",
                 "BILL_AMT1",
                 "BILL_AMT2",
                 "BILL_AMT3", 
                 "BILL_AMT4",
                 "BILL_AMT5",
                 "BILL_AMT6",
                 "PAY_AMT1",
                 "PAY_AMT2",
                 "PAY_AMT3",
                 "PAY_AMT4",
                 "PAY_AMT5",
                 "PAY_AMT6"
),


Credit.Numeric <- Credit %>% select(c(1,5,6:23))

#Numerical Summary 2
Credit.Numeric %>%  
  apply(MARGIN= 2,FUN = summary) %>%
  round(1) %>%
  kable(caption = paste0("Summary of Numeric Predictors", species),
        row.names = TRUE)

library(corrplot)
corrplot( cor(Credit.Numeric))


library(plotly)
plot_ly (
  x = Credit$"PAY_0",
  y = Credit$"AGE",
  z = Credit$"LIMIT_BAL",
  color = ~Credit$"DEFAULT", colors = c('red', 'blue'),
  type = 'scatter3d' ,
  mode = 'markers',
  marker = list(size = 2))

plot_ly(
  x = DEFAULT,
  type = 'histogram')
)


##### Model Fitting

### Test and Train Set
### Tune p
p <- .8
Train <- sample(1:nrow(Credit), p*nrow(Credit))
Credit.Train <- Credit[Train,]
Credit.Test <- Credit[-Train,]

#### Logistic Regression
### Tune the number of variables and the threshold
glm.fit <- glm(DEFAULT ~ ., Credit.Train, family = binomial)
summary(glm.fit)

threshold <- 0.5
glm.prob.train <- predict(glm.fit, type = "response", newdata = Credit.Train)
glm.pred.train <- ifelse(glm.prob.train>0.5, 1, 0)
glm.pred.train <- factor(glm.pred.train, levels=c(1,0), labels = c("Yes","No"))
glm.trainMiss <- sum(glm.pred.train==Credit.Train$DEFAULT, na.rm=T)/length(Credit.Train$DEFAULT)

glm.prob.test <- predict(glm.fit, type = "response", newdata = Credit.Test)
glm.pred.test <- ifelse(glm.prob.test>0.5, 1, 0)
glm.pred.test <- factor(glm.pred.test, levels=c(1,0), labels = c("Yes","No"))
glm.testMiss <- sum(glm.pred.test==Credit.Train$DEFAULT, na.rm=T)/length(Credit.Train$DEFAULT)

##### Decision Tree

library(tree)

fullfit <- tree(DEFAULT~.,data=Credit.Train)
fullfit.summ <- summary(fullfit)
print(fullfit.summ)
fullfit.summ$misclass
pruneFit <- cv.tree(fullfit, FUN = prune.misclass)
best <- pruneFit$size[which.min(pruneFit$dev)]
pruneFitFinal <- prune.misclass(fullfit, best = best)
pruneFit.summ <- summary(pruneFitFinal)
pruneFit.Miss.train <- pruneFit.summ$misclass


pruneFit.pred.test <- predict(pruneFitFinal, newdata = Credit.Test %>% select(-DEFAULT), type = "class")
prune.testMiss <- 1- sum(pruneFit.pred.test==Credit.Train$DEFAULT, na.rm=T)/length(Credit.Train$DEFAULT)

##### Random Forest
### User choose k
library(caret)
k <- 5

p <- ncol(Credit.Train)-1
tuning <- data.frame(mtry = c(sqrt(p),p/3,1:5))
rfFit <- train(DEFAULT ~ ., data = Credit.Train, method ="rf",trControl = trainControl(method="cv", number=k), tuneGrid = tuning)

rfFit
