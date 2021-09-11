####mean imputation
setwd("/cloud/project/complete pentacam parameter")
all=read.csv("data_machine learning.csv")
all=all[,-c(1,3,4)]
colnames(all)[1]="group"
colnames(all)[1:10]
e=c()
for (i in 2:812){
  a=sum(is.na(all[,i]))
  if (a>0){
    e=c(e,i)
  }
}
#102
all1=all
for(i in 2:812){
  all1[is.na(all1[,i]), i]=round(mean(all1[,i], na.rm = TRUE),2)
}

e=c()
for (i in 2:812){
  a=sum(is.na(all1[,i]))
  if (a>0){
    e=c(e,i)
  }
}

##PCA
colnames(all1)
corneal_power_pca_resut<- prcomp(all1[,2:812], center = TRUE, scale = TRUE)
summary(corneal_power_pca_resut)
data=data.frame(group = all1$group, corneal_power_pca_resut$x)
colnames(data)

#######train and test
smp_size <- floor(0.7 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

#####select the optimal component set
train$group=as.factor(train$group)
rfoutput=data.frame("combination"="0","Mean_accuracy"=0,"Mean_sensitivity"=0,"Mean_specificity"=0,"Mean_presicion"=0,stringsAsFactors=T)
ctrl<-trainControl(method = "cv",number =10,summaryFunction = multiClassSummary)
maxacc=0
for (i in 3:268){
  da=train[,1:i]
  print(colnames(da))
  set.seed(7)
  rfCVFit<-train(group~., data =da, method = "rf", trControl = ctrl, metric='Accuracy')
  Mean_acc=max(rfCVFit$results$Accuracy)
  index=which(rfCVFit$results$Accuracy==Mean_acc)
  sensitivity=(rfCVFit$results$Sensitivity)[index]
  specificity=(rfCVFit$results$Specificity)[index]
  precision=(rfCVFit$results$Precision)[index]
  rfoutput <- rbind(rfoutput, data.frame("combination" = toString(colnames(da[,-1])), "Mean_accuracy" =Mean_acc,"Mean_sensitivity"=sensitivity,"Mean_specificity"=specificity,"Mean_presicion"=precision))
  if (Mean_acc>maxacc){
    maxacc=Mean_acc
    predictors=i
  }
  print(maxacc)
  print(predictors)
}
#write.csv(rfoutput[-1,],"rf_pca1.csv",na="",row.names = F)

da=train[,1:16]
set.seed(7)
rfCVFit<-train(group~., data =da, method = "rf", trControl = ctrl, metric='Accuracy')
rfCVFit
a=predict(rfCVFit$finalModel, newdata=test)
table(a,test$group)
