library(dplyr)
library(data.table)
train_data<-read.csv("train.csv")

train_data_without_missingage<-filter(train_data,!is.na(Age))

train_data_table<-data.table(train_data)
c2<-train_data[!is.na(train_data$Age),]

nrow(filter(train_data_without_missingage,Fare==0))

sample1<-sample(train_data_without_missingage,size=150,replace = FALSE)

index<-sample(1:nrow(train_data_without_missingage),0.5(train_data_without_missingage))

x<-train_data_without_missingage
  
k<-sample(1:nrow(train_data_without_missingage), 450, replace=FALSE)
train1<-train_data_without_missingage[k,]
test<-train_data_without_missingage[-k,]
                                    
model<-glm(Survived~.,data=train1[,-c(1,4,9,11)],family=binomial(link='logit'))

pred<-predict(model,test[,-c(1,4,9,11,2)],type='response')

fitted.results <- ifelse(pred> 0.5,1,0)
fitted.results1<-as.data.frame(fitted.results)

misClasificError <- mean((fitted.results1)!= test$Survived)
print(paste('Accuracy',1-misClasificError))

x<-train_data_without_missingage
View(x)
x$Cabin[x$Cabin==""]<-"NA"
