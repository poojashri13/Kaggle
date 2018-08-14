library(rpart)
install.packages("rpart.plot")
library(ggplot2)
library(rpart.plot)
setwd("E:\\kaggle\\titanic")
titanic_train <- read.csv("train.csv")
titanic_test <- read.csv("test.csv")
titanic_train$Survived<-as.factor(titanic_train$Survived)
titanic_train$Pclass<-as.factor(titanic_train$Pclass)
titanic_train$Name <-as.character(titanic_train$Name)

tree_model1 <- rpart(Survived~Sex +Pclass +Embarked,titanic_train)#accuracy 77.9%

titanic_test$Pclass <- as.factor(titanic_test$Pclass)
titanic_test$Survived <- predict(tree_model1,titanic_test,type = "class")
write.csv(titanic_test[,c("PassengerId","Survived")],"submission_tree_model1.csv",row.names = F)
summary(tree_model1)

tree_model2 <- rpart(Survived~Sex +Pclass +Embarked +Age,titanic_train)#77.03%

titanic_test$Pclass <- as.factor(titanic_test$Pclass)
titanic_test$Survived <- predict(tree_model2,titanic_test,type = "class")
write.csv(titanic_test[,c("PassengerId","Survived")],"submission_tree_model2.csv",row.names = F)


tree_model3 <- rpart(Survived~Sex +Pclass,titanic_train)#76.55

titanic_test$Pclass <- as.factor(titanic_test$Pclass)
titanic_test$Survived <- predict(tree_model3,titanic_test,type = "class")
write.csv(titanic_test[,c("PassengerId","Survived")],"submission_tree_model3.csv",row.names = F)


tree_model4 <- rpart(Survived~Sex +Pclass +Embarked+Age +SibSp+Parch+Ticket+Fare,titanic_train)#76.55

titanic_test$Pclass <- as.factor(titanic_test$Pclass)
titanic_test$Survived <- predict(tree_model4,titanic_test,type = "class")
write.csv(titanic_test[,c("PassengerId","Survived")],"submission_tree_model4.csv",row.names = F)


