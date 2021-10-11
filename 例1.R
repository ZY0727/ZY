install.packages("neuralnet")
library("neuralnet")
BuyOrNot<-read.table(file="C:/Users/20691/Desktop/BuyOrNot.txt",header=TRUE)

################
# Purchase (0: 没有购买  1：购买)
#Gender（1:男 2：女）
#收入水平Income(1:高收入 2：中收入 3：低收入)
################

##########neurealnet??��????????
set.seed(12345)
(BPnet1<-neuralnet(Purchase~Age+Gender+Income,data=BuyOrNot,hidden=2,err.fct="ce",linear.output=FALSE))
BPnet1$result.matrix
BPnet1$weight
BPnet1$startweights 

########Ȩֵ???????ӻ?
plot(BPnet1)
#######??????��??Ҫ?Լ????ӻ?
head(BPnet1$generalized.weights[[1]])
par(mfrow=c(2,2))
gwplot(BPnet1,selected.covariate="Age")
gwplot(BPnet1,selected.covariate="Gender")
gwplot(BPnet1,selected.covariate="Income")

##########??ͬ??????��ˮƽ?????µ?Ԥ??
newData<-matrix(c(39,1,1,39,1,2,39,1,3,39,2,1,39,2,2,39,2,3),nrow=6,ncol=3,byrow=TRUE)
new.output<-compute(BPnet1,covariate=newData)
new.output$net.result

############ȷ?????ʷָ?ֵ
install.packages("ROCR")
library("ROCR")
detach("package:neuralnet")
summary(BPnet1$net.result[[1]])
pred<-prediction(predictions=as.vector(BPnet1$net.result),labels=BPnet1$response)
par(mfrow=c(2,1))
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,print.cutoffs.at=c(0.2,0.45,0.46,0.47))
perf<-performance(pred,measure="acc")
plot(perf)
Out<-cbind(BPnet1$response,BPnet1$net.result[[1]])
Out<-cbind(Out,ifelse(Out[,2]>0.468,1,0))
(ConfM.BP<-table(Out[,1],Out[,3]))
(Err.BP<-(sum(ConfM.BP)-sum(diag(ConfM.BP)))/sum(ConfM.BP))

##############nnet??��??????????????
install.packages("nnet")
library("nnet")
set.seed(1000)
(BPnet2<-nnet(Purchase~Age+Gender+Income,data=BuyOrNot,size=2,entropy=TRUE,abstol=0.01))
predict(BPnet2,BuyOrNot,type="class")
library("neuralnet")
set.seed(1000)
(BPnet3<-neuralnet(Purchase~Age+Gender+Income,data=BuyOrNot,
  algorithm="backprop",learningrate=0.01,hidden=2,err.fct="ce",linear.output=FALSE))

