library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)
train <-read.csv(file.choose())
View(train)
attach(train)
str(train)

train$workclass<-as.factor(workclass)
train$education<-as.factor(education)
train$occupation<-as.factor(occupation)
train$relationship<-as.factor(relationship)
train$educationno <-as.integer(as.factor(educationno))
train$native<-as.factor(native)
train$race<-as.factor(race)
train$Salary<-as.factor(Salary)
train$sex<-as.factor(sex)
train$maritalstatus<-as.factor(maritalstatus)
str(train)
summary(train)
test <-read.csv(file.choose())
str(test)
View(test)
attach(test)
##Converting into factor
test$workclass<-as.factor(workclass)
test$education<-as.factor(education)
test$occupation<-as.factor(occupation)
test$relationship<-as.factor(relationship)
test$educationno <-as.integer(as.factor(educationno))
test$native<-as.factor(native)
test$race<-as.factor(race)
testSalary<-as.factor(Salary)
test$sex<-as.factor(sex)
test$maritalstatus<-as.factor(maritalstatus)

##Plot
ggplot(data=train,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
##generating model.
Model <- naiveBayes(train$Salary ~ ., data = train)
summary(Model)
model_pred <-predict(Model,test$Salary)
confusionMatrix(table(model_pred,test$Salary))

?naiveBayes

lapl <- naiveBayes(train$Salary ~ ., data = train,laplace=5,type="raw")
summary(lapl)
model_pred1 <-predict(lapl,test$Salary)
mean(model_pred1!=test$Salary)
confusionMatrix(table(model_pred1,test$Salary))

library(rpart)
rpartNB <-rpart(Salary~.,data=train,method="class", control = rpart.control(cp = 0))
summary(rpartNB)
printcp(rpartNB)
plotcp(rpartNB)
pruneNB<-prune(rpartNB,cp=0.0084)

                    
summary(pruneNB)
test$pred <- predict(pruneNB, test$Salary, type = "class")
###boosting
mod1 <-train(Salary~.,data=train,method="ada",trControl=trainControl("repeatedcv",number=2))
mod1
prada <-predict(mod1,cdtest)
mean(prada==cdtest$cdata1)
a<-table(cdtrain$cdata1,prada)
confusionMatrix(cdtest$cdata1,prada)
CrossTable(cdtest$cdata1)
