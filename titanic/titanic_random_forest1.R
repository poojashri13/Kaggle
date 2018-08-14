
library(ggplot2)       #visualization
library(ggthemes)      #visualization
library(scales)        #visualization
library(dplyr)         #data manipulation
library(mice)          #imputation
library(randomForest)  #classification algorithm
setwd("E:\\kaggle\\titanic")
train<-read.csv("train.csv",stringsAsFactors = F)
test <- read.csv("test.csv",stringsAsFactors = F)
test$Survived <- numeric(nrow(test))
full  <- rbind(train, test) # bind training & test data

# check data
str(full)
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

full$Surname <- sapply(full$Name,function(x) strsplit(x,split = '[,.]')[[1]][[1]])

full$FSize <- full$SibSp + full$Parch + 1
full$Family <- paste (full$Surname, full$FSize,sep = '_')
ggplot(full[1:891,],aes(x = FSize ,fill = factor(Survived))) + geom_bar(stat = 'count',position = 'dodge') + scale_x_continuous(breaks=c(1:11)) + labs(x='Family Size') + theme_few()
full$FSizeD[full$FSize == 1] <- 'single'
full$FSizeD[full$FSize >1 && full$FSize <5] <- 'small'
full$FSizeD[full$FSize > 4] <-"large"
full$Embarked[63] <- 'C'
full$Survived<-as.factor(full$Survived)
full$Sex<-as.factor(full$Sex)
full$Title<-as.factor(full$Title)
full$Embarked<-as.factor(full$Embarked)
full$FSizeD<-as.factor(full$FSizeD)
full[1044, ]
full$Fare[is.na(full$Fare)] <- median(full$Fare,na.rm = T)
full$Age [is.na(full$Age)] <- -1
full$Embarked[is.na(full$Embarked)]<-'C'
train_new<- full[1:891,]
test <- full[892:1309,]
set.seed(500)
rf_model <- randomForest(Survived ~ Pclass + Sex + Title + Age + SibSp +FSizeD + Parch + Fare + Embarked, train_new, ntree = 100)
rf_model


importance <-importance(rf_model)
prediction<- predict(rf_model ,test)
write.csv(test_new[,c("PassengerId","Survived")],"submission_Random_forest2.csv",row.names = F)
