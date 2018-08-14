setwd("E:\\kaggle\\titanic")
install.packages("tidyr")
install.packages("dplyr")
install.packages("doParallel")
library(doParallel)
cluster<-makeCluster(detectCores())
registerDoParallel(cluster)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
train <- tbl_df(read.csv("train.csv", stringsAsFactors = FALSE))
summary(train)
test<-tbl_df(read.csv("test.csv", stringsAsFactors = FALSE))
summary(train$Name)
test$Survived<- 0
titanic<- rbind(train,test)
titanic$Survived<-factor(titanic$Survived)
titanic$Pclass<-factor(titanic$Pclass)
titanic$Sex<-factor(titanic$Sex)
titanic$SibSp<-factor(titanic$SibSp)
titanic$Parch<-factor(titanic$Parch)
summary(titanic)
titanic$Embarked<-factor(titanic$Embarked)
titanic$Embarked[(titanic$Embarked)==""]<-'S'
titanic$Name<-as.character(titanic$Name)
titanic[is.na(titanic$Fare),]
table(titanic$Sex,titanic$Embarked)
table(titanic$Survived,titanic$Sex)
preobj<-preProcess(titanic[,c("Age","Sex","Parch","SibSp","Fare","Pclass","Embarked")],method = c("bagImpute"))
preobj$bagImp$Age$model$mtrees
summary(preobj)
titanic1<-predict(preobj,titanic[,c("Age","Sex","Parch","SibSp","Fare","Pclass","Embarked")])
dim(titanic1)
titanic1$Survived <-titanic$Survived
titanic1$Age[is.na(titanic1$Age)==T]

extractTitle<-function(x){
  title<-str_trim(strsplit(x,split='[,.]')[[1]][[2]])
  if(title %in%c('Mme','Mlle'))
    return('Mlle')
  else if(title %in% c('Dona','Lady',"the Countess"))
    return ('Lady')
  else if(title %in% c('Capt','Don','Major','Sir','Jonkheer','Dr'))
    return ('Sir')
  else
    return(title)
    
}
titanic1$Title<-sapply(titanic$Name,FUN=extractTitle)
str(titanic1)
titanic1$Title[titanic1$Sex == "female" & titanic1$Title ="Dr"] = "Ms"
table(titanic1$Sex,titanic1$Title)
