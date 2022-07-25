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

# Define server logic required to draw a histogram
server <- function(input, output) {
    
      output$img <- renderUI({
      tags$img(src = "https://www.jbhe.com/wordpress/wp-content/uploads/2018/12/claflin.png")
  })

    
    reactiveDF<-reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
       
        df$predictions<-predict(irisModel, newdata = df[,3:22], type ="class")
        return(df)
        
    })
    
    output$mytable = DT::renderDataTable({
        req(input$file1)
        
        return(DT::datatable(reactiveDF(),  options = list(pageLength = 100), filter = c("top")))
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(reactiveDF(), file, row.names = FALSE)
        }
    )
    

    
}
