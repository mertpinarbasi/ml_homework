library(xgboost)
require(caTools)
library(caret)


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

# sampling 
set.seed(101) 
sample = sample.split(df_income$income, SplitRatio = .70)
trainingSet = subset(df_income, sample == TRUE)
testSet  = subset(df_income, sample == TRUE)

# isolaate y cariable 
Y_train <- trainingSet$income
Y_test <- testSet$income

#isolate x cariable 
X_train <-  subset(trainingSet,select=-c(income))
X_test <- subset(testSet,select=-c(income))

# one hot encoding for train set 


dmy <- dummyVars(" ~ .", data = X_train, fullRank = T)
X_train <- data.frame(predict(dmy, newdata = X_train))
dmyTest <- dummyVars(" ~ .", data = X_train, fullRank = T)
X_test <- data.frame(predict(dmy, newdata = X_test))

#state parameters
parameters <-list(eta = 0.3 ,
                  max_depth= 6,
                  subsample=1,colsample_bytree=1
                  ,min_child_weight=1,
                  gamma=0,set.seed=1502
                  ,eval_metric="auc",
                  objective="binary:logistic",
                  booster="gbtree")

#run xgboost
model <- xgboost(data= as.matrix( X_train),label = Y_train,set.seed(1502),nround=50,params=parameters,verbose=1)

#evaluate model 
predictions = predict(model , newdata = as.matrix(X_test))
predictions = ifelse(predictions>0.5,1,0)

#check accuracy
confusionMatrix(table(predictions,as.matrix(Y_test)))

xgb.plot.shap(data =as.matrix(X_test),model = model,top_n =5 )
