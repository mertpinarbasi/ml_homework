library(xgboost) # decision tree library
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
dmyTest <- dummyVars(" ~ .", data = X_test, fullRank = T) #?
X_test <- data.frame(predict(dmyTest, newdata = X_test))

#state parameters



best_param = list()
best_seednumber = 1234
best_auc = Inf
best_auc_index = 0

for (iter in 1:100) {
  
  
  param <-list(eta = 0.3 ,
                    max_depth= 6,
                    subsample=1,colsample_bytree=1
                    ,min_child_weight=1,
                    gamma=0,set.seed=best_seednumber
                    ,eval_metric="auc",
                    objective="binary:logistic",
                    booster="gbtree")
  
  
  cv.nround = 100
  cv.nfold = 10
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
 
  
  
  mdcv <- xgb.cv(data= as.matrix( X_train), label = Y_train,params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround, early_stopping_rounds = 6,
                 verbose = T,maximize=FALSE)
  
  
  max_auc_index = mdcv$best_iteration
  max_auc = mdcv$evaluation_log[max_auc_index]$test_auc_mean
 
  
  
  if (max_auc> best_auc) {
    best_auc = max_auc
    best_auc_index = max_auc_index
    best_seednumber = seed.number
    best_param = param
  }
  
  
}

nround = best_auc_index
set.seed(best_seednumber)

#run xgboost
 model <- xgboost(data= as.matrix( X_train),label = Y_train,set.seed(best_seednumber),nround=1000,params=best_param,verbose=1)

#evaluate model 
predictions = predict(model , newdata = as.matrix(X_test))
predictions = ifelse(predictions>0.5,1,0)

#check accuracy
confusionMatrix(table(predictions,as.matrix(Y_test)))

xgb.plot.shap(data =as.matrix(X_test),model = model,top_n =5 )


##########################

#run xgboost
model <- xgboost(data= as.matrix( X_train),label = Y_train,set.seed(1502),nround=1000,params=best_param,verbose=1)

#evaluate model 
predictions = predict(model , newdata = as.matrix(X_test))
predictions = ifelse(predictions>0.5,1,0)

#check accuracy
confusionMatrix(table(predictions,as.matrix(Y_test)))
