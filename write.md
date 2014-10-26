library(caret)
trainingraw <- read.table("pml-training.csv",sep=",",na.strings = c("NA",""),header=TRUE)
testing <- read.table("pml-testing.csv",sep=",",na.strings = c("NA",""),header=TRUE)

inTrain <- createDataPartition(trainingraw$classe, p=0.70, list=FALSE)
training <- trainingraw
validation <- trainingraw[-inTrain,]

training<-training[,colSums(is.na(training)) == 0]
classe<-training$classe
nums <- sapply(training, is.numeric)
training<-cbind(classe,training[,nums])
training$X<-training$num_window<-NULL

validation<-validation[,colSums(is.na(validation)) == 0]
vclasse<-validation$classe
vnums <- sapply(validation, is.numeric)
validation<-cbind(vclasse,validation[,vnums])
colnames(validation)[1]<-"classe"
validation$X<-validation$num_window<-NULL

testing<-testing[,colSums(is.na(testing)) == 0]
tnums <- sapply(testing, is.numeric)
testing<-testing[,tnums]
testing$X<-testing$num_window<-NULL


#Fit a model using random forest, running in parallel with 8 processes on i7 the training of the model took ~22 minutes.

library(doMC)
registerDoMC(cores = 8)
fit <- train(training$classe~.,data=training, method="rpart")
save(fit,file="fit.RData")
load(file = "./fit.RData")
fit$results
#Error estimation with cross validation

#Using the model that we've trained, we're performing a cross validation with the rest of data from the dataset reserved for this reason. The out of error rate is expected to be less than 1%, as the accuracy of the model observed above is 99.88%.

traincontrol <- trainControl(method = "cv", number = 5)
fit_crossvalidation <- train(validation$classe~.,data=validation, method="rf",trControl=traincontrol)
save(fit_crossvalidation,file="fit_crossvalidation.RData")
load(file="./fit_crossvalidation.RData")
fit_crossvalidation$resample
fit_crossvalidation$results
confusionMatrix(predict(fit_crossvalidation, newdata=validation), validation$classe)
#Indeed, by calculating the out of sample error (the cross-validation estimate is an out-of-sample estimate) we get the value of 0.54%:
  
fit_crossvalidation$finalModel
#Predict the 20 test cases

#Finally, to predict the classe of the testing dataset, we're applying the prediction using the model we've trained and output the results in the respective files as adviced by the instructor:
  
test_prediction<-predict(fit, newdata=testing)
test_prediction
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(test_prediction)
