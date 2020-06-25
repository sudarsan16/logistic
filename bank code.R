## installing the packages and libraries
install.packages("moments")
library(moments)
library(ROSE)
library(DMwR)
library(DataExplorer)
library(corrplot)
library(ROCR)

## importing the dataset 

bank<-read.csv("C:\\Users\\God\\Desktop\\Assignments\\8 - Logistic\\bank-full.csv")
attach(bank)
head(bank)
str(bank)

##Basic statistics and EDA part

## dummy variables 

bank$job<-as.integer(as.character(factor(bank$job,levels = c("admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed",
                                                    "services","student","technician","unemployed","unknown"),
                                labels = c(1,2,3,4,5,6,7,8,9,10,11,12))))

bank$marital<-as.integer(as.character(factor(bank$marital,levels = c("divorced","married","single"),labels = c(0,1,2))))

bank$education<-as.integer(as.character(factor(bank$education,levels = c("primary","secondary","tertiary","unknown"),labels = c(1,2,3,4))))

bank$housing<-as.integer(as.character(factor(bank$housing,levels = c("no","yes"),labels = c('0','1'))))

bank$loan<-as.integer(as.character(factor(bank$loan,levels = c("no","yes"),labels = c(0,1))))

bank$month<-as.integer(as.character(factor(bank$month,levels = c("apr","aug","dec","feb","jan","jul","jun","mar","may","nov","oct","sep"),
                                 labels = c(1,2,3,4,5,6,7,8,9,10,11,12))))

bank$poutcome<-as.integer(as.character(factor(bank$poutcome,levels = c("failure","other","success","unknown"),labels = c(1,2,3,4))))

bank$contact<-as.integer(as.character(factor(bank$contact,levels = c("cellular","telephone","unknown"),labels = c(1,2,3))))

bank$y<-as.integer(as.character(factor(bank$y,levels=c("yes","no"),labels = c(0,1))))   ## 0 label as yes & 1 label as No

## data wrangling 

summary(bank)
str(bank)

sapply(bank,skewness)
sapply(bank,kurtosis)

table(y)
prop.table(table(y))5

## imbalance dataset 

im.bank<-bank
head(im.bank)
str(bank.both)

bank.both$job<-as.integer(bank.both$job)    ## changing factor to integer
bank.both$marital<-as.integer(bank.both$marital)
bank.both$education<-as.integer(bank.both$education)
bank.both$default<-as.integer(bank.both$default)
bank.both$housing<-as.integer(bank.both$housing)
bank.both$loan<-as.integer(bank.both$loan)
bank.both$contact<-as.integer(bank.both$contact)
bank.both$month<-as.integer(bank.both$month)
bank.both$poutcome<-as.integer(bank.both$poutcome)
bank.both$y<-as.integer(bank.both$y)

bank.both<-ovun.sample(y~., data = im.bank, method = "both",seed = 111, p = 0.5, N = 45211)$data
x<-table(bank.both$y)
prop.table(x)
attach(bank.both)

over.bank<-ovun.sample(y~.,data = im.bank, method = "over", N = 79844 )$data     ## to find N we need calculate No*2  
x2<-table(over.bank$y)
prop.table(x2)

under.bank<-ovun.sample(y~., data = im.bank, method = "under", N =10578 )$data       ## To find N we need calculate with yes*2
x3<-table(under.bank$y)
prop.table(x3)

rose.bank<-ROSE(y~., data = im.bank, N=10000, seed = 111)$data
table(rose.bank$y)

## graphical view 
 
plot_bar(bank.both)
plot_histogram(bank.both)
plot_correlation(bank.both)

colnames(bank.both)
col<-bank.both[,c("age","balance","campaign","day","duration","pdays","previous")]
corr<-cor(col)
corrplot(corr,method = "color",addCoef.col = "black")   ## checking correlation

## model building 


bank.model1<-glm(y~.,data = bank,family = "binomial")

bank.model2<- glm(y~age+marital+education+balance+housing+loan+contact+month+duration+campaign+pdays+previous+poutcome,
                  data = bank, family = "binomial")

bank.both.mod3<-glm(y~.,data = bank.both,family = "binomial")


bank.both.mod4<- glm(y~age+marital+education+default+balance+housing+loan+contact+month+duration+campaign+pdays+previous+poutcome+day,
                  data = bank.both, family = "binomial")

##summary about model building

summary(bank.model1)
summary(bank.model2)
summary(bank.both.mod3)
summary(bank.both.mod4)

## prediction 

bank.model1.fit<-predict(bank.model1,type = "response")
bank.model2.fit<-predict(bank.model2,type = "response")
bank.both.fit<-predict(bank.both.mod3,type = "response")
bank.both.fit2<-predict(bank.both.mod4,type = "response")

## misclassification error 

pred.model1<-ifelse(bank.model1.fit>0.5,1,0)
table.model1<-table(pred.model,bank$y)

pred.model2<-ifelse(bank.model2.fit>0.5,1,0)
table.model2<-table(pred.model2,bank$y)

pred.both<-ifelse(bank.both.fit>0.5,1,0)
table.both<-table(pred.both,bank.both$y)

pred.both2<-ifelse(bank.both.fit2>0.5,1,0)
table.both2<-table(pred.both2,bank.both$y)

## confusion matrix 

acc.model1<-sum(diag(table.model1)/sum(table.model1))
acc.model1    ## 0.8909% accuracy

TPR.model1<-39136/(786+39136)   ## 98% TPR
FPR.model1<-4144/(4144+1145)    ## 78% FPR

acc.model2<-sum(diag(table.model2)/sum(table.model2)) ## 89%
TPR.model2<-39142/(786+39142)   ## 98% TPR
FPR.model2<-4148/(4148+1141)    ## 78% FPR

acc.both<-sum(diag(table.both)/sum(table.both)) ##80% 
TPR.both<-18398/(4230+18398)   ## 81% TPR
FPR.both<-4518/(4518+18065)    ## 20% FPR

acc.both2<-sum(diag(table.both2)/sum(table.both2)) ##80% 
TPR.both2<-18403/(4225+18403)   ## 81% TPR
FPR.both2<-4521/(4521+18062)    ## 20% FPR

##final model based on the best accuracy

bank.both.fit2<-predict(bank.both.mod4,type = "response")
pred.both2<-ifelse(bank.both.fit2>0.5,1,0)
table.both2<-table(pred.both2,bank.both$y)
acc.model2<-sum(diag(table.model2)/sum(table.model2)) ## 89%
TPR.model2<-39142/(786+39142)   ## 98% TPR
FPR.model2<-4148/(4148+1141)    ## 78% FPR

## ROC  curve 

rocp.bothf2<-prediction(bank.both.fit2,bank.both$y)
rocper.bothf2<-performance(rocp.model1,'tpr','fpr')
plot(rocper.bothf2,colorize=T)
abline(a = 0,b = 1)

## identify the best value 
rocper<-performance(rocp.bothf2,'acc')
max<-which.max(slot(rocper.bothf2,"y.values")[[1]])
acc<-slot(rocper.bothf2,"y.values")[[1]][max]
cut<-slot(rocper.bothf2,"x.values")[[1]][max]
