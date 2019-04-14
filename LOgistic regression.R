data(readr)
library(readxl)
dataset<-read_excel(file.choose())
install.packages('DataExplorer')
library(DataExplorer)
str(dataset)

#Exploratory data Ananlysis
datasett<-dataset[-1]
str(datasett)
datasett$`Oil Leakage`<-as.factor(dataset$`Oil Leakage`)
datasett$`Fuel supply`<-as.factor(dataset$`Fuel supply`)
datasett$`Vector Group`<-as.factor(dataset$`Vector Group`)
datasett$Insulation<-as.factor(datasett$Insulation)
dataset$`Energy Losses`<-as.factor(datasett$`Energy Losses`)
datasett$`Pressure Relay`<-as.factor(datasett$`Pressure Relay`)
datasett$`Cooling Operation`<-as.factor(datasett$`Cooling Operation`)
datasett$Bushing<-as.factor(datasett$Bushing)
datasett$`Over Current Protection (OC)`<-as.factor(datasett$`Over Current Protection (OC)`)
datasett$`Fire Fighting Systems (FFS)`<-as.factor(datasett$`Fire Fighting Systems (FFS)`)
datasett$`Silica Gel Color`<-as.factor(datasett$`Silica Gel Color`)
datasett$Outage<-as.factor(datasett$Outage)
#structure of dataset
str(datasett)
#plot variables
plot_str(datasett)
#plot_missing
plot_missing(datasett)
sum(is.na(datasett))
#pakcages 
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
basic_eda(datasett)
freq(datasett)
plot_num(datasett)
plot_histogram(datasett)
plot_density(datasett)
plot_correlation(datasett, type = 'continuous','Review.Date')
plot_bar(datasett)
boxplot(datasett)
summary(datasett)
dim(datasett)

#radomdise
set.seed(1234)
#PARTITITOON DATASET into traning and testing
sampling<-sample(2,nrow(datasett),replace = T,prob=c(0.8,0.2))
train<-datasett[sampling==1,]
test<-datasett[sampling==2,]
dim(train)
dim(test)

#model building
attach(datasett)
logistic_model<-glm(Outage~`Lamination thickness maintainence`+`Life time`+`Oil Leakage`+`Fuel supply`+`Vector Group`+Insulation+`Energy Losses`+`Pressure Relay`+Core+`Cooling Operation`+`Primary Voltage`+`Secondary Voltage`+`Overload protection`+Bushing+`Over Current Protection (OC)`+`Fire Fighting Systems (FFS)`+`Hot Spots (HS)`+`Breakdown voltage`+`Water content`+`Oil Acidity`+`Silica Gel Color`+`Tap Changer`+`Percentage Impedance (at 75°C)`,data=datasett,family = "binomial")
logistic_model
#summary of model
summary(logistic_model)

#Prediction
p1<-predict(logistic_model,train,type='response')
head(p1)
head(train)
pred1<-ifelse(p1>0.5,1,0)
tab<-table(pred1,train$Outage)
1-sum(diag(tab))/sum(tab)

#missclassfication error
p2<-predict(logistic_model,test,type='response')
p2
pred2<-ifelse(p2>0.5,1,0)
tab1<-table(pred2,test$Outage)
tab1
1-sum(diag(tab))/sum(tab)


#goodness of fit test
with(logistic_model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))

#model Performance Evaluation
library(ROCR)
predd<-prediction(pred2,test$Outage)
eval<-performance(predd,"acc")
plot(eval)

#idenitify the best value
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
acc
cut<-slot(eval,"y.values")[[1]][max]
print(c(Accuray=acc,Cutoff=cut))

#ROC Curve
pred<-predict(logistic_model,datasett$Outage)
predROC<-prediction(pred,datasett$Outage)
performanceROC<-performance(predROC,"tpr","fpr")
plot(performanceROC,colorize=T)
abline(a=0,b=1)
auc.tmp <- performance(predROC,"auc"); 
auc <- as.numeric(auc.tmp@y.values)
auc
