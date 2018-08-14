setwd("E:\\Kaggle\\titanic")
titanic_train <- read.csv("train.csv",header = T)
titanic_test <-read.csv("test.csv",header = T)
head(titanic_train)
titanic_train$Sex = as.factor(titanic_train$Sex)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_train$Survived = as.factor(titanic_train$Survived)
str(titanic_train)
class(titanic_train)
titanic_train$Sex == "male"
male<-filter(titanic_train,Sex == "male",Survived ==1)
survived_male<-select(male,Sex,Survived,Pclass,Embarked)
nrow(survived_male$Pclass)
head(survived_male)
summary(titanic_train$Sex)
summary(titanic_train$Pclass)
xtabs(~Survived+Sex+Pclass+Embarked,titanic_train)
x11()
library(ggplot2)

ggplot(titanic_train) + geom_bar(aes(x=Sex, fill=Survived)) + facet_grid(Pclass ~ .)
ggplot(titanic_train) + geom_bar(aes(x = Sex, fill = Survived)) + facet_grid(Embarked~Pclass )
x11()
ggplot(titanic_train) + geom_bar(aes(x = Sex, fill = Survived)) + facet_grid(Pclass~Embarked )
n <- numeric(nrow(titanic_test))
n
for(i in 1:nrow(titanic_test)){
  if(titanic_test[i,"Sex"] == "female"){
    if(titanic_test[i,"Pclass"] == 1){
      if(titanic_test[i,"Embarked"] == "C" | titanic_test[i,"Embarked"] == "S"){
        n[i] = 1
      }
      else if(titanic_test[i,"Embarked"] == "Q"){
        n[i] = 0
      }
    }
    else if(titanic_test[i,"Pclass"] == 2){
      n[i] = 1;
    }
    else if(titanic_test[i,"Pclass"] == 3){
      
        n[i] = 1
      
    }
    
  }
  else if(titanic_test[i,"Sex"] == "male"){
    if(titanic_test[i,"Pclass"] == 1){
      n[i] = 1
    }
    else{
      n[i] = 0
    }
  }
}
titanic_test$Survived <- n
write.csv(titanic_test[,c("PassengerId","Survived")],"submission_titanic1.csv",row.names = F)