
library(tidyverse)
##此包用于数据加载及预处理，没有安装的先install.packages("tidyverse")
#读取数据
Training_Data <- read_csv("C:\\Users\\20691\\Desktop\\信用风险分析\\Training Data.csv")
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
as.factor(TrainData$income)
as.factor(TrainData$age)
TrainData$married[TrainData$married=="single"]<-0
TrainData$married[TrainData$married=="married"]<-1
view(TrainData$married)
TrainData$married<-factor(TrainData$married,levels=c(0,1),labels=c("single","married"))
#####
TrainData$house_ownership[TrainData$house_ownership=="norent_noown"]<-0
TrainData$house_ownership[TrainData$house_ownership=="rented"]<-1
TrainData$house_ownership[TrainData$house_ownership=="owned"]<-2
view(TrainData$house_ownership)
TrainData$house_ownership<-factor(TrainData$house_ownership,levels=c(0,1,2),labels=c("norent_noown","rented","owned"))

#######
TrainData$car_ownership[TrainData$car_ownership=="no"]<-0
TrainData$car_ownership[TrainData$car_ownership=="yes"]<-1
view(TrainData$car_ownership)
TrainData$car_ownership<-factor(TrainData$car_ownership,levels=c(0,1),labels=c("no","yes"))

######
#as.factor(TrainData$city)
#as.factor(TrainData$pression)
#as.factor(TrainData$state)
as.factor(TrainData$current_job_years)
as.factor(TrainData$current_house_years)
as.factor(TrainData$risk_flag)#因变量
######
#初步考察各自变量与自变量的显著性
modelfull<-glm(risk_flag~income+age+experience+married+house_ownership+car_ownership+current_job_years+current_house_years,data=TrainData,family=binomial())
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
Training_Data <- read_csv("C:\\Users\\20691\\Desktop\\信用风险分析\\Training Data.csv")
TrainData1<-Training_Data[,-(5:10)]#删除非数值型变量
View(TrainData1)
df<-TrainData1[,2:6]#只有数值型变量可以进行
df1=cor(df)
kappa(df1,exact=T)
#[1] 4.655059#小于100，说明多重共线性很小
###
summary(modelboth)
exp(cbind("OR"=coef(modelboth),confint(modelboth)))

#剔除income，及current_house_years这两个变量,建立模型modelreduce
modelreduce<-glm(risk_flag~age+married+experience+house_ownership+car_ownership+current_job_years,data=TrainData,family=binomial())
summary(modelreduce)
anova(modelfull,modelreduce,test="Chisq")
#从两套模型的AIC值及运用anova()函数对它们的比较，尽管income，
#及current_house_years这两个变量在统计意义上不显著，但其实对于判断贷款人
#是否违约是有参考意义的

#评价预测变量对结果概率的影响（参考课本P287）testdata$pro<-predict(modelfull,newdata=testdata,type="response")
testdata<-data.frame(income=mean(TrainData$income),age=seq(20,80,10),experience=mean(TrainData$experience),marriedmarried=mean(TrainData$marriedmarried),house=mean(TrainData$house_ownership),car=mean(TrainData$car_ownership),job=mean(TrainData$current_job_years),houselive=mean(TrainData$current_house_years))

testdata$pro<-predict(modelfull,newdata=testdata,type="response")


##检测过渡离势
deviance(modelfull)/df.residual(modelfull)
#[1] 0.742199(为毛是这个值呀，这不太好解释呀，后续同学看这个模型哪里需要修改)














#叶
#1.变量处理
#广义线性模型
any(is.na(d))#是否有缺失值
###转换为因子  
as.factor(d$income)
as.factor(d$age)

d$married[d$married=="single"]<-0
d$married[d$married=="married"]<-1
view(d$married)
d$married<-factor(d$married,levels=c(0,1),labels=c("single","married"))

d$house_ownership[d$house_ownership=="norent_noown"]<-0
d$house_ownership[d$house_ownership=="rented"]<-1
d$house_ownership[d$house_ownership=="owned"]<-2
view(d$house_ownership)
d$house_ownership<-factor(d$house_ownership,levels=c(0,1,2),labels=c("norent_noown","rented","owned"))

d$car_ownership[d$car_ownership=="no"]<-0
d$car_ownership[d$car_ownership=="yes"]<-1
view(d$car_ownership)
d$car_ownership<-factor(d$car_ownership,levels=c(0,1),labels=c("no","yes"))


as.factor(d$current_job_years)
as.factor(d$current_house_years)
as.factor(d$risk_flag)#因变量
#2.logistic建模
########
modelfull<-glm(risk_flag~income+age+experience+married+house_ownership+car_ownership+current_job_years+current_house_years,data=d,family=binomial())
summary(modelfull)
model2<-glm(risk_flag~age+experience+married+house_ownership+car_ownership+current_job_years,data=d,family=binomial())
summary(model2)

####两个模型是否有显著差异
anova(modelfull,model2,test='Chisq')
#结果显示两个模型无显著差异

#3.模型信息
#or
exp(coef(model2))
nagelkerke(model2)#加载rcompanion包

#4.作图 
####作图  加载rms包
#残线图
ddist<-datadist(d)
options(datadist='ddist')
model3<-lrm(risk_flag~age+experience+married+house_ownership+car_ownership+current_job_years,x=T,y=T)
nom<-nomogram(model3,fun=plogis,funlable='Risk Rate',lp=F)
plot(nom)

#矫正曲线
x<-model3$x
y<-model3$y
rm(x,y)
#library(rms)、library(VGAM)
cal<- rms::calibrate(model3,method='boot',B=500,bw=T,rule='p',sls=0.05)
plot(cal)

##汪
TrainData<-Training_Data[,-1]#删除第一列数据（即Id这个变量，因为这个变量没有实际意义）
#按照7：3的比例抽取训练集与测试集
Data <- sample(2, nrow(TrainData), replace = TRUE, prob=c(0.7, 0.3))
train_data <- TrainData[Data == 1,]
test_data <- TrainData[Data == 2,]

head(TrainData)#查看新数据TrainData的部分值

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



#郑
#（变量之间是否有多重共线性？有时间精力就可以试试）
Training_Data <- read_csv("C:\\Users\\20691\\Desktop\\信用风险分析\\Training Data.csv")
TrainData1<-Training_Data[,-(5:10)]#删除非数值型变量
View(TrainData1)
df<-TrainData1[,2:6]#只有数值型变量可以进行
df1=cor(df)
kappa(df1,exact=T)
#[1] 4.655059#小于100，说明多重共线性很小


install.packages("rms")


