## installing the packages and library
library(ROSE)
library(DMwR)
library(DataExplorer)
library(caret)
library(e1071)
##importing the dataset

aff<-read.csv("C:\\Users\\God\\Desktop\\Assignments\\8 - Logistic\\affairs.csv")
head(aff)
str(aff)
attach(aff)

## data wrangling 

summary(aff)

sum(is.na(aff$aff.airs))    ## there is no missing value

## creating dummy variables 

aff['Gender']<-as.integer(factor(aff$gender,levels = c('male','female'),labels = c(1,2)))  ##creating dummy variable for gender
aff['Childern']<-as.integer(factor(aff$children,levels = c('no','yes'),labels = c(1,2)))  ##creating dummy variable for childern
aff['aff.airs']<-ifelse(affairs>1,'1','0')
aff = subset(aff, select = -c(gender,children,affairs) )
head(aff)
attach(aff)

## imbalanced dataset

table(aff.airs)   ## checking the total values of categorical variable

prop.table(table(aff.airs))

over.aff<-ovun.sample(aff.airs~.,data = aff,method = "over",N = 970)
table(over.aff.result$aff.airs)

under.aff<-ovun.sample(aff.airs~.,data=aff,method="under",N=232)$data
table(under.aff$aff.airs)

both.aff<-ovun.sample(aff.airs~.,data=aff,method="both",N=601,seed = 222,p = 0.5)$data
table(both.aff$aff.airs)

summary(both.aff)
attach(both.aff)
head(both.aff)

## graphical view

plot_intro(both.aff)  ## intro about continuous and discrete columns
plot_missing(both.aff)  ## visualisation missing value 
plot_bar(both.aff)      ## for discrete variable using bar plot
plot_histogram(both.aff)
plot_correlation(both.aff)
#plot_boxplot(box,by = "age")
#box<-both.aff[,c("age","yearsmarried","religiousness","education","occupation","rating")]


## model building 

model.aff<-glm(factor(both.aff$aff.airs)~.,data = both.aff,family = "binomial")
summary(model.aff)

## final model after removing influence factors

model.aff2<-glm(factor(aff.airs)~age+yearsmarried+religiousness,data = both.aff,family = "binomial")
summary(model.aff2)

## misclassification error

fit.aff<-predict(model.aff2,type = "response")
pred2<-ifelse(fit.aff>0.5,1,0)
tab<-table(pred2,both.aff$aff.airs)
tab

Accuracy<-sum(diag(tab)/sum(tab))
Accuracy                          ## 67 % accuracy 
Error <- 1-Accuracy
Error

219/(219+87)  ##TPR 71%
113/(113+182) ##FPR 38%

both.aff['pred']<-pred2   ## adding the predicted value in original dataset

# ROC Curve 

library(ROCR)
rocrpred<-prediction(fit.aff,both.aff$aff.airs)
rocrperf<-performance(rocrpred,'tpr','fpr')

plot(rocrperf,colorize=T)
abline(a=0,b=1)

## area under the curve

auc<-performance(rocrpred,"acc")
auc<-unlist(slot(auc,"cutoffs"))
auc
