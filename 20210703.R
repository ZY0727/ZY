install.packages("carData")
library(car)
install.packages("pROC")
library("pROC")

set.seed(1)

BuyOrNot<-read.table(file="C:/Users/20691/Desktop/BuyOrNot.txt",header=TRUE)
BuyOrNot[1:3,]

train_sub = sample(nrow(BuyOrNot),6/10*nrow(BuyOrNot))
traindata=BuyOrNot[train_sub,]
testdata=BuyOrNot[-train_sub,]

#BuyOrNot_logistic <- glm(Purchase ~ Age + Gender + Income,data = traindata, family = "binomial")
#summary(BuyOrNot_logistic)

BuyOrNot_logistic <- glm(Purchase ~ Gender + Income,data = traindata, family = "binomial")
summary(BuyOrNot_logistic)

m1=glm(Purchase~.,family=binomial,data=data.frame(delay=ytrain,xtrain))
summary(m1)

pre_logistic <- as.numeric(predict(BuyOrNot_logistic,newdata = testdata,type = "response")>0.5)
obs_p_log = data.frame(prob=pre_logistic,obs=testdata$Purchase)
table(testdata$Purchase,pre_logistic,dnn=c("真实值","预测值"))
roc(testdata$Purchase,pre_logistic)

plot(roc(testdata$Purchase,pre_logistic),print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE,main='逻辑回归ROC曲线')
