library(caret); library(ggplot2)
### The first part of the project is reading the training and test sets.
trainData <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA",""))
testData <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA",""))

### Then we remove the columns with missing values
trainData<-trainData[,colSums(is.na(trainData)) == 0]
testData <- testData[,colSums(is.na(testData)) == 0]
trainData$X<-trainData$num_window<-NULL
testData$X<-testData$num_window<-NULL
####feature selectin
nums <- sapply(trainData, is.numeric)
trainData1 <- trainData[ , nums]
testData1 <- testData[,nums]


###remove the covariates with zero standard deviation
#nsv <- nearZeroVar(trainData1, saveMetrics = TRUE)
#nsv <- nearZeroVar(trainData1, saveMetrics = FALSE)

#trainData1 <- trainData1[,-nsv]
#testData1 <- testData1[,-nsv]
pp <- preProcess (trainData1, method = c("center","scale", "pca"), thresh=0.9)
training <- predict(pp, trainData1)
testing <- predict(pp, testData1)


training$classe <- as.factor(trainData$classe)


fitControl <- trainControl(## 10-fold CV,
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

modFit <- train(classe ~., data= training, method="rpart", preProcess="pca")

result <- predict(modFit, testing)




confusionMatrix(predict(modFit, training), training$classe)
#missclass <- missClass((predict(modFit, training), training$classe)
missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}                       


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
