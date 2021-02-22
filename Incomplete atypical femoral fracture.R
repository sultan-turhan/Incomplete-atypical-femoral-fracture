read.csv(file = ("involvement.csv"), sep = ";",  header = T)
involvement <- read.csv(file = ("involvement.csv"), sep = ";",  header = T)

library("caret")
partition <- createDataPartition(involvement$INVOLVEMENT,p=0.75,list = FALSE)
train_data <- involvement[partition,]
test_data <-involvement[-partition,]

library("mccr")
mccr(involvement$INVOLVEMENT, testPC)

fitControl <- trainControl(
  method = 'cv',
  number = 5,search = "random")  
#GLM
LR<-train(as.factor(INVOLVEMENT)~., data=train_data, method="glm", family="binomial", control = list(maxit = 100),trControl = fitControl,positive='1')
importance<-varImp(LR)$importance
options(warn=-1)
testPC <- predict(LR, test_data)
options(warn=1)
plot(varImp(LR))
confusionMatrix(as.factor(test_data$INVOLVEMENT), testPC,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testPC)
summary(LR)
plot(varImp(LR))

#RANDOM FOREST
rf <- train(as.factor(INVOLVEMENT)~.,data=train_data, method = "rf",trControl = fitControl)
importancerf<-varImp(rf)$importance
testrf <- predict(rf, test_data, prob=TRUE)
confusionMatrix(as.factor(test_data$INVOLVEMENT), testrf,positive='1',mode = "everything")
mccr(??nvolvoment$INVOLVEMENT, testrf)plot(rf)
summary(rf)
plot(varImp(rf))
rf$finalModel$ntree

#rpart
rpart <- train(as.factor(INVOLVEMENT)~., data=train_data, method = "rpart",trControl = fitControl)
importancerpart<-varImp(rpart)$importance 
testrpart <- predict(rpart, test_data)
confusionMatrix(as.factor(test_data$INVOLVEMENT), testrpart,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testrpart)
summary(rpart)
plot(varImp(rpart))
rpart$finalMode$control

#wsrf
wsrf <- train(as.factor(INVOLVEMENT)~., data=train_data, method = "wsrf",trControl = fitControl)
importancewsrf<-varImp(wsrf)$importance 
testwsrf <- predict(wsrf, test_data)
confusionMatrix(as.factor(test_data$INVOLVEMENT), testwsrf,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testwsrf)
summary(wsrf)
plot(varImp(wsrf))
wsrf$finalModel

#adaboost
adaboost <- train(as.factor(INVOLVEMENT)~., data=train_data, method = "adaboost",trControl = fitControl)
importanceadaboost<-varImp(adaboost)$importance 
testadaboost <- predict(adaboost, test_data)
confusionMatrix(as.factor(test_data$INVOLVEMENT), testadaboost,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testadaboost)
summary(adaboost)
plot(varImp(adaboost))
adaboost$finalModel
  
#XGBOOST
xgboost <- train(as.factor(INVOLVEMENT)~., data=train_data, method = "xgbTree",trControl = fitControl)
importancexgboost <-varImp(xgboost)$importance 
testxgboost <- predict(xgboost, test_data)
confusionMatrix(as.factor(test_data$INVOLVEMENT), testxgboost,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testxgboost)
summary(xgboost)
plot(varImp(xgboost))
xgboost$finalModel
  
#SMOTE
library("DMwR")
library(performanceEstimation())
data <- involvement[, c(1:15)]
table(data$INVOLVEMENT)
data$INVOLVEMENT=factor(data$INVOLVEMENT)
newData <- SMOTE(INVOLVEMENT~., data, perc.over = 200,perc.under=200)
table(newData$INVOLVEMENT)
View(newData)
partition <- createDataPartition(newData$INVOLVEMENT,p=0.75,list = FALSE)
train_datas <- newData[partition,]
test_datas <-newData[-partition,]

#SMOTE GLM
sonuclarLR<-train(as.factor(INVOLVEMENT)~., data=train_datas, method="glm", family="binomial", control = list(maxit = 100),trControl = fitControl)
summary(sonuclarLR)
importance<-varImp(sonuclarLR)$importance
options(warn=-1)
testPC <- predict(sonuclarLR, test_datas)
options(warn=1)
plot(varImp(sonuclarLR))
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testPC,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testPC)
summary(sonuclarLR)
plot(varImp(sonuclarLR))

#SMOTE RANDOM FOREST
rf <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "rf",trControl = fitControl)
importancerf<-varImp(rf)$importance 
testrf <- predict(rf, test_datas)
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testrf,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testrf)
summary(rf)
plot(varImp(rf))
rf$finalModel$ntree

#SMOTE rpart
rpart <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "rpart",trControl = fitControl)
importancerpart<-varImp(rpart)$importance 
testrpart <- predict(rpart, test_datas)
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testrpart,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testrpart)
summary(rpart)
plot(varImp(rpart))
rpart$finalModel$control

#SMOTE wsrf
wsrf <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "wsrf",trControl = fitControl)
importancewsrf<-varImp(wsrf)$importance 
testwsrf <- predict(wsrf, test_datas)
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testwsrf,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testwsrf)
summary(wsrf)
plot(varImp(wsrf))

#SMOTE adaboost
adaboost <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "adaboost",trControl = fitControl)
importanceadaboost<-varImp(adaboost)$importance 
testadaboost <- predict(adaboost, test_datas)
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testadaboost,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testadaboost)
summary(adaboost)
plot(varImp(adaboost))

#SMOTE XGBOOST
xgboost <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "xgbTree",trControl = fitControl)
importancexgboost <-varImp(xgboost)$importance 
testxgboost <- predict(xgboost, test_datas)
confusionMatrix(as.factor(test_datas$INVOLVEMENT), testxgboost,positive='1',mode = "everything")
mccr(involvement$INVOLVEMENT, testxgboost)
summary(xgboost)
plot(varImp(xgboost))

#Lime
library(lime)
partition <- createDataPartition(newData$INVOLVEMENT,p=0.75,list = FALSE)
train_datas <- newData[partition,]
test_datas <- newData[-partition,]
rflime <- train(as.factor(INVOLVEMENT)~., data=train_datas, method = "rf",trControl = fitControl)
explainer <- lime(train_datas,rflime)
explanation1 <- explain(test_datas[21,], explainer, n_labels = 1, n_features = 3, quantile_bins=T)
plot_features(explanation1)
explanation2 <- explain(test_datas[31,], explainer, n_labels = 1, n_features = 3,quantile_bins=T)
plot_features(explanation2)

library(gridExtra)
grid.arrange(plot_features(explanation1), plot_features(explanation2), ncol=2, nrow =1, widths=c(1.3,1.3))

#Variable Importance
library(ggthemes)
library(ggplot2)

read.csv(file = ("vi.csv"), sep = ";",  header = T)
tablo1 <- read.csv(file = ("vi.csv"), sep = ";",  header = T)
Factors<-tablo1$Factors
Imports <-tablo1$Imports
Algoritmalar1 <-tablo1$Algoritmalar

ggplot(tablo1, aes(x = Algoritmalar1, y = Imports)) +
  labs( y="Feature Import", x="Risk Factors")+
  theme(text = element_text(size = 12))+
  geom_point(aes(col=Factors),shape=21, color="black", fill="blue", size=4)
