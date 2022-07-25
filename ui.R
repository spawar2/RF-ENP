library(shiny)
library(DT)
library(tidyverse)
#irisModel <- readRDS("www/Model.rds")

Data <- read.csv("https://raw.githubusercontent.com/spawar2/RF-ENP/main/Training-Toxicity.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
library(randomForest)
library(caret)
set.seed(222)
ind <- sample(2, nrow(Data[,3:22]), replace = TRUE, prob = c(0.7, 0.3))
train <- Data[,2:22][ind==1,]
test <- Data[,2:22][ind==2,]
irisModel <- randomForest(Toxicity~., data= train, proximity=TRUE, , ntree=100, mtry=2, importance=TRUE) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Engineered nanoparticles (ENPs) Toxicity Prediction Tool"),
  uiOutput("img"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Upload csv file here (format should be same as sample data)",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), 
            
            
            # Button
            downloadButton("downloadData", "Download the Predictions")
        ),

        # Show the table with the predictions
        mainPanel(
            DT::dataTableOutput("mytable")
        )
    )
)

