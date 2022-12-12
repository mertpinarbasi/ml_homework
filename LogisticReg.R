library(caTools)
library(caret)
library(gains)
library(pROC)


dataset <-read.csv("income_evaluation.csv")


# fnlwgt row remowed since it is not necessary 
df_income <- subset(dataset,select=-c(fnlwgt))

# turn binary attribute into 0 and 1 
df_income$income <-ifelse(df_income$income==" >50K",1,0)

df_income$native.country
apply(X=df_income,2,FUN=function(x) length(which(x==' ?')))

# handling missing values 


#define function to calculate mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}


mod_workclass_df  = find_mode(df_income$workclass)
mod_occupation_df  = find_mode(df_income$occupation)
mod_country_df  = find_mode(df_income$native.country)

# replacing the missing values with the mod values
df_income$workclass[df_income$workclass == ' ?'] <- mod_workclass_df
df_income$occupation[df_income$occupation == ' ?'] <- mod_occupation_df
df_income$native.country[df_income$native.country == ' ?'] <- mod_country_df





# knn uses euclid distance which is highly sensitive to measurements 
# some numeric values might be oversized influence 
# to fix this problem scaling need to be applied

# scaling 
library(dplyr)
df_income <-df_income %>%  mutate(across(c(where(is.numeric), -income), scale))


# one hot encoding 

dmy <- dummyVars(" ~ .", data = df_income, fullRank = T)
df_income <- data.frame(predict(dmy, newdata = df_income))




# sampling 
set.seed(101) 
dataIndex <- createDataPartition(df_income$income,p=0.60,list=FALSE)
trainSet <- df_income[dataIndex,]
validationSet <- df_income[-dataIndex,]


# k-fold cross validation 

# control settings
ctrlSetting <- trainControl(method="cv",number=3)
grid<- expand.grid(.k=c(1:3)) # store k results in grid 

# converting target feature to categorical data
trainSet$income <-as.factor(trainSet$income)
validationSet$income<-as.factor(validationSet$income)


set.seed(12)
KNN_fit<-train(income~.,data=trainSet,method="glm",trControl=ctrlSetting,family=binomial(link = logit),metric="Accuracy")
KNN_class<-predict(KNN_fit,newdata=validationSet)
confusionMatrix(KNN_class,validationSet$income,positive="1")
