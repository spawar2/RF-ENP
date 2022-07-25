# Pawar, July 06, Purpose: Testing RF on nano-particle classifications
# Read data

setwd("/Users/pawar/Desktop/")
Data <- read.csv("https://raw.githubusercontent.com/spawar2/RF-ENP/main/Training-Toxicity.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

# Training RF for outcome Apopotosis

library(randomForest)
library(caret)
set.seed(222)

# Fold split

ind <- sample(2, nrow(Data[,3:22]), replace = TRUE, prob = c(0.7, 0.3))
train <- Data[,2:22][ind==1,]
test <- Data[,2:22][ind==2,]

# Train on all avaliable data

rf <- randomForest(Toxicity~., data= train, proximity=TRUE, , ntree=100, mtry=2, importance=TRUE) 
p1 <- predict(rf, test)
confusionMatrix(factor(round(p1, digits = 0)), factor(test$Toxicity))

# Test on external data
TData <- read.csv("External-Test.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
TData_1 <- TData[,2:22]

p1 <- predict(rf, TData_1)
confusionMatrix(factor(round(p1, digits = 0)), factor(TData$Toxicity))

saveRDS(rf, "Model.rds")

# R Shiny App


