library(rpart)
setwd("E:\\kaggle\\titanic")
titanic_train <- read.csv("train.csv")
titanic_test <- read.csv("test.csv")
titanic_test$Survived <- numeric(nrow(titanic_test))
data <- rbind(titanic_train,titanic_test)
dim(data)
data$Age[is.na(data$Age)] <- -1
data$Embarked[is.na(data$Embarked)] <-"S"
data$Fare[is.na(data$Fare)] <-median(data$Fare, na.rm = T)
data$Sex <- as.factor(data$Sex)
data$Embarked <-as.factor(data$Embarked)
data$Pclass <- as.factor(data$Pclass)
data$Survived <- as.factor(data$Survived)
train_new <- data[1:891,]
test_new <- data[892:1309,]
str(train_new)
str(test_new)
tree_model <- rpart(Survived~ Age +Sex +Pclass + Embarked + Fare,train_new,method = "class")##75.5
summary(tree_model)
test_new$Survived <- predict(tree_model,test_new,type ="class")
write.csv(test_new[,c("PassengerId","Survived")],"submission_tree_model.csv",row.names = F)
