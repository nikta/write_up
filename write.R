library(caret); library(ggplot2)
### The first part of the project is reading the training and test sets.
trainData <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA",""))
testData <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA",""))

### Data inspection and feature selection, and pre-processing
trainData<-trainData[,colSums(is.na(trainData)) == 0]
testData <- testData[,colSums(is.na(testData)) == 0]
trainData$X<-trainData$num_window<-NULL
testData$X<-testData$num_window<-NULL

nums <- sapply(trainData, is.numeric)
trainData1 <- trainData[ , nums]
testData1 <- testData[,nums]

training <- trainData1
testing <- testData1
training$classe <- as.factor(trainData$classe)

###### We use density plots and feature plot to select the right features. Since there are so many features we use the feature plot and select a subset of columns each time, and example is shown below.Columns 2,33,34,35,47,48 do not present much information as the density plot is centered around one single value
qplot(training[,2], data=training, geom="density" )
featurePlot(x=training[,c(1:5)], y=training$classe,plot="pairs")
##featurePlot(x=training[,c(5:10)], y=training$classe,plot="pairs")
training <- training[,c(-2,-33,-34,-35,-47,-48)]
testing <- testing[,c(-2,-33,-34,-35,-47,-48)]

##We ispect in there are any columns with zero variance in the data. There is non.

nsv <- nearZeroVar(trainData1, saveMetrics = TRUE)
nsv <- nearZeroVar(trainData1, saveMetrics = FALSE)

##We select k-fold corss validation.
fitControl <- trainControl(## 10-fold CV,
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

## we use random forests or decision tree to model the data
library(doMC)
registerDoMC(cores = 24)
modFit <- train(classe ~., data= training, method="rf", preProcess="pca", trControl = fitControl)
#modFit <- train(classe ~., data= training, method="rf", trControl = fitControl)

result <- predict(modFit, testing)

####To uncerstand the accuracy of the model. We can also detemine the accuracy by sing miss-classification function.
confusionMatrix(predict(modFit, training), training$classe)

missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}                       
missclass <- missClass(predict(modFit, training), training$classe)

####The last part of the project is to write the result in single files for submission.
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
