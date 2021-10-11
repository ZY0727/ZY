library(tidyverse)
##此包用于数据加载及预处理，没有安装的先install.packages("tidyverse")
#读取数据
Training_Data <- read_csv("D:/R/Training Data.csv")
#查看数据
view(Training_Data)
#查看数据变量
names(Training_Data)
#################################
#数据预处理及描述统计
library(tidyverse)
##此包用于数据加载及预处理，没有安装的先install.packages("tidyverse")
is.na(Training_Data)#查看是否存在缺失值
#结果返回FALSE，表明Training_Data不存在缺失值
TrainData<-Training_Data[,-1]#删除第一列数据（即Id这个变量，因为这个变量没有实际意义）
#按照7：3的比例抽取训练集与测试集
Data <- sample(2, nrow(TrainData), replace = TRUE, prob=c(0.7, 0.3))
train_data <- TrainData[Data == 1,]
test_data <- TrainData[Data == 2,]
head(TrainData)#查看新数据TrainData的部分值
#查看样本数据的年龄分布
hist(TrainData$age,  breaks=6,col="blue",xlab="age",main="Age distribution of TrainData")
#查看职业分布情况
tableProfession<-table(TrainData$profession)
tableProfession
#查看样本所在州分布情况
tableState<-table(TrainData$state)
tableState
#查看样本数据的收入分布
hist(TrainData$income,  breaks=12,col="blue",xlab="Income",main="Income distribution of TrainData")
############

as.factor(train_data$income)
as.factor(train_data$age)
train_data$married[train_data$married=="single"]<-0
train_data$married[train_data$married=="married"]<-1
train_data$married<-factor(train_data$married,levels=c(0,1),labels=c("single","married"))

#####
train_data$house_ownership[train_data$house_ownership=="norent_noown"]<-0
train_data$house_ownership[train_data$house_ownership=="rented"]<-1
train_data$house_ownership[train_data$house_ownership=="owned"]<-2
train_data$house_ownership<-factor(train_data$house_ownership,levels=c(0,1,2),labels=c("norent_noown","rented","owned"))

#######
train_data$car_ownership[train_data$car_ownership=="no"]<-0
train_data$car_ownership[train_data$car_ownership=="yes"]<-1
train_data$car_ownership<-factor(train_data$car_ownership,levels=c(0,1),labels=c("no","yes"))

######
#as.factor(train_data$city)
#as.factor(train_data$pression)
#as.factor(train_data$state)
as.factor(train_data$current_job_years)
as.factor(train_data$current_house_years)
as.factor(train_data$risk_flag)#因变量
######
#初步考察各自变量与自变量的显著性
modelfull<-glm(risk_flag~income+age+experience+married+house_ownership+car_ownership+current_job_years+current_house_years,data=train_data,family=binomial())
summary(modelfull)#模型结果
#计算OR值及可信区间
exp(cbind("OR"=coef(modelfull),confint(modelfull)))
#论文写作：结果的解释一般是：罗列各个自变量P值，OR值，及可信区间（三线表）
#再解释各个值所表示的信息
#结果表示（看P值）：income,current_house_years对方程的贡献不显著
#其中income变量不显著与生活经验不太相符合，下面用逐步向后回归法筛选变量

######逐步回归筛选自变量
#逐步回归stepAIC()函数在MASS包中
#install.packages("MASS")
library(MASS)
modelboth<-stepAIC(modelfull,direction="both")#向后法逐步回归
#只迭代一次便停止，且包含了income，及current_house_years这两个变量

#为减小误差。接着用向前法逐步回归
modelforward<-stepAIC(modelfull,direction="forward")
#向前与向后法逐步回归筛选出来的自变量量相同，且AIC值相同


#（变量之间是否有多重共线性？有时间精力就可以试试）

###
summary(modelboth)
exp(cbind("OR"=coef(modelboth),confint(modelboth)))

#剔除income，及current_house_years这两个变量,建立模型modelreduce
modelreduce<-glm(risk_flag~age+married+experience+house_ownership+car_ownership+current_job_years,data=train_data,family=binomial())
summary(modelreduce)
anova(modelfull,modelreduce,test="Chisq")
#从两套模型的AIC值及运用anova()函数对它们的比较，尽管income，
#及current_house_years这两个变量在统计意义上不显著，但其实对于判断贷款人
#是否违约是有参考意义的

#评价预测变量对结果概率的影响（参考课本P287）testdata$pro<-predict(modelfull,newdata=testdata,type="response")
testdata<-data.frame(income=mean(train_data$income),age=seq(20,80,10),experience=mean(train_data$experience),marriedmarried=mean(train_data$marriedmarried),house=mean(train_data$house_ownership),car=mean(train_data$car_ownership),job=mean(train_data$current_job_years),houselive=mean(train_data$current_house_years))

testdata$pro<-predict(modelfull,newdata=testdata,type="response")
##检测过渡离势
deviance(modelfull)/df.residual(modelfull)
#[1] 0.742199

##计算准确率
library(pROC)
pr <- predict(modelfull,newdata= test_data,type = c("response"))
pr2<-predict(modelfull,data=train_data,type=c('response'))
#计算模型内的准确率
roccurve1 <- roc(train_data$risk_flag~ pr2)
auc(roccurve1)
plot.roc(roccurve1,xlim = c(1,0),ylim=c(0,1))
#计算模型外的准确率
roccurve <- roc(test_data$risk_flag~pr)
auc(roccurve)
plot.roc(roccurve,xlim = c(1,0),ylim=c(0,1))





