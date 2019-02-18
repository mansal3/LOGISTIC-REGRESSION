#import the data
library(readr)
bank <- read_delim("Downloads/bank-full (2).csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
View(bank)
#structure the data 
head(bank)
str(bank)
#remove all the na
apply(bank,2,function(x){sum(is.na(x))})
for (x in names(bank)) {
  if (is.factor(bank[[x]]))
  {print(paste (x,":",levels(bank[[x]])))}
}
#factor the output variable
bank$y<-factor(ifelse(bank$y=="no",0,1))
#apply linear regreesion check the value
linearmodel<-lm(y~.,data = bank)
#apply logistic regreesion
model<-glm(y~.,data = bank,family = binomial())
#summary
summary(model)
#determine the expoential 
exp(coef(model))
#predict the model
prob<-predict(model,type="response",bank)
prob
#confusion matrix
confusion<-table(prob>0.4,bank$y)
confusion
#check accuray
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
#90.6%auucray
#roc curve
install.packages("ROCR")
library(ROCR)
rocpredict<-prediction(prob,bank$y)
rocpreformance<-performance(rocpredict,'tpr','fpr')
plot(rocpreformance,colorize=T)
auc<-paste(c("AUC="),round(as.numeric(performance(rocpredict,"auc")@y.values),digits = 2),sep = "")
#found more area under roc cover more is the area better is the logistic regression