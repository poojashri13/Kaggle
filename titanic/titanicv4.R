setwd("E:\\kaggle\\titanic")
titanic_test = read.csv("test.csv")
dim(titanic_test)
str(titanic_test)

titanic_test$Survived = ifelse(titanic_test$Pclass == 1,1,0)

result = titanic_test[,c("PassengerId","Survived")]
write.csv(result,"submission3.csv",row.names = F)
