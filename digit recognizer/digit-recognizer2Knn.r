library(caret)
library(doParallel)

c1<-makeCluster(4)
registerDoParallel(c1)


setwd("E:\\kaggle\\digit recognizer")
train<-read.csv("train.csv")

train$label<-as.factor(train$label)

train1<-train[,-1]
var<-nearZeroVar(train1,saveMetrics = T,allowParallel = T)
train1<-train1[,var$zeroVar==F]
dim(train1)
pca<-preProcess(train1,method=c("pca"))
pca$rotation
train2<-predict(pca,train1)
dim(train2)
tr_ctrl1<-trainControl(method="cv",verboseIter = T)
tn_grid<-data.frame(.k=c(4,5))
model_knn<-train(x= train2,train$label,method="knn",trControl=tr_ctrl1,tuneGrid=tn_grid)
stopCluster(c1)
test<-read.csv("test.csv")
dim(test)
test1<-test[,var$zeroVar==F]
dim(test1)
test2<-predict(pca,test1)
test2$label=predict(model_knn,test2)
test2$ImageId<-1:nrow(test2)
submission <-test2[,c("ImageId","label")]
write.table(submission,file="submission1.csv",col.name=T,row.names = F,sep=",")
