library(rpart)
library(mice)
library(randomForest)
setwd("E:\\kaggle\\titanic")
titanic_train <- read.csv("train.csv")
titanic_test <- read.csv("test.csv")
titanic_test$Survived <- numeric(nrow(titanic_test))
data <- rbind(titanic_train,titanic_test)
dim(data)

##--------Embarked--------------
data$Embarked[data$Embarked ==""] <- 'C'

data[62,]
# Grab title from passenger names
#-------------Name character---------------
data$Name <- as.character(data$Name)
data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)

# Show title counts by sex
table(data$Sex, data$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
data$Title[data$Title == 'Mlle']        <- 'Miss' 
data$Title[data$Title == 'Ms']          <- 'Miss'
data$Title[data$Title == 'Mme']         <- 'Mrs' 
data$Title[data$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(data$Sex, data$Title)
data$Title<-as.factor(data$Title)
data$Surname <- sapply(data$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
#full$Surname <- sapply(full$Name,function(x) strsplit(x,split = '[,.]')[[1]][[1]])
data$FSize <- data$SibSp + data$Parch +1 
data$Family <- paste(data$Surname,data$FSize,sep ='_')
#-------------Fare-----------------------
data$Fare[is.na(data$Fare)] <-median(data$Fare, na.rm = T)
#---------------Sex-------------------

data$Sex <- as.factor(data$Sex)
#--------------Embarked Factor--------------------
data$Embarked <-as.factor(data$Embarked)
#-----------Pclass Factor---------------
data$Pclass <- as.factor(data$Pclass)
#----------------Survived Factor----------
data$Survived <- as.factor(data$Survived)


#----------Missing age values 

mice_mod <- mice(data[,!names(data) %in% c("PassengerId","Name","Ticket","Cabin","Family","Surname","Survived")],method = "rf")
mice_output <- complete(mice_mod)
data$Age <- mice_output$Age
sum(is.na(data$Ticket))
str(data)
train_new <- data[1:891,]
test_new <- data[892:1309,]
str(train_new)
str(test_new)
set.seed(500)
is.na(data$Family)
rf_model <- randomForest(Survived~Pclass + Sex + Age + Embarked + FSize + Fare +Title +SibSp +Parch,train_new )
summary(rf_model)
rf_model$final
test_new$Survived <- predict(tree_model,test_new,type ="class")
write.csv(test_new[,c("PassengerId","Survived")],"submission_random_Forest.csv",row.names = F)
