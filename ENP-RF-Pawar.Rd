# Pawar, Jan 27, Purpose: Testing RF on nano-particle classifications

# Read data
setwd("/Users/pawar/Desktop/IRCN-R01-RFP")
Data <- read.csv("Data.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

#3-7 columns miss forest imputation for missing values
library(missForest)

Data_IMP <- missForest(Data[,3:7])

Imputed_Data <- data.frame(Data[,2], Data_IMP$ximp, Data[,c(8:58)])

# Training RF for outcome Apopotosis
library(randomForest)
library(caret)
set.seed(222)

# Below fold split wont work with such small training data
ind <- sample(2, nrow(Imputed_Data[,2:9]), replace = TRUE, prob = c(0.7, 0.3))
train <- Imputed_Data[,2:9][ind==1,]
test <- Imputed_Data[,2:9][ind==2,]

# Train on all avaliable data
data_Apopotosis <- data.frame(Imputed_Data[,2:6], Imputed_Data[,27:37])
rf <- randomForest(Apoptosis~., data= data_Apopotosis, proximity=TRUE, , ntree=100, mtry=2, importance=TRUE) 
print(rf)

test <- data_Apopotosis[11:16,]
p1 <- predict(rf, test)
confusionMatrix(factor(round(p1, digits = 0)), factor(test$Apoptosis))

########################
Confusion Matrix and Statistics

          Reference
Prediction 0 1
         0 2 0
         1 0 4
                                     
               Accuracy : 1          
                 95% CI : (0.5407, 1)
    No Information Rate : 0.6667     
    P-Value [Acc > NIR] : 0.08779    
                                     
                  Kappa : 1          
                                     
 Mcnemar's Test P-Value : NA         
                                     
            Sensitivity : 1.0000     
            Specificity : 1.0000     
         Pos Pred Value : 1.0000     
         Neg Pred Value : 1.0000     
             Prevalence : 0.3333     
         Detection Rate : 0.3333     
   Detection Prevalence : 0.3333     
      Balanced Accuracy : 1.0000     
                                     
       'Positive' Class : 0  
########################
