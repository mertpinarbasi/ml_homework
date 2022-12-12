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

# converting target feature to categorical data
df_income$income <-as.factor(df_income$income)




# sampling 
set.seed(101) 
dataIndex <- createDataPartition(df_income$income,p=0.70,list=FALSE)
trainSet <- df_income[dataIndex,]
validationSet <- df_income[-dataIndex,]



# k-fold cross validation 

# control settings
ctrlSetting <- trainControl(method="cv",number=20)
grid<- expand.grid(.k=c(1:5)) # store k results in grid 

set.seed(213123)
KNN_fit<-train(income~.,data=trainSet,method="knn",trControl=ctrlSetting,tuneGrid=grid)
KNN_class<-predict(KNN_fit,newdata=validationSet)
confusionMatrix(KNN_class,validationSet$income,positive="1")


validationSet$income <- as.numeric(as.character())