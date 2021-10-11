library(tidyverse)
Training_Data <- read_csv("C:\\Users\\20691\\Desktop\\信用风险分析\\Training Data.csv")
TrainData<-Training_Data[,-1]
Data <- sample(2, nrow(TrainData), replace = TRUE, prob=c(0.7, 0.3))
train_data <- TrainData[Data == 1,]
test_data <- TrainData[Data == 2,]
install.packages("neuralnet")
library(neuralnet)
trainset$0<-trainset$risk_flag=="0"
trainset$1<-trainset$risk_flag=="1"
attach(TrainData)
network<-neuralnet(0+1~income+age+experience+married+house_ownership+car_ownership+profession+city+state+current_house_years+current_job_years,train_data,hidden=3)
network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)
par(mfrow=c(2,2))
gwplot(network,selected.covariate = "income")
#
net.predict<-compute(network,test_data[-1])$net.result
net.prediction<-c("0","1")[apply(net.predict,1,which.max)]
predict.table<-table(test_data$Species,net.prediction)
predict.table
install.packages("caret")
library(caret)
confusionMatrix(predict.table)