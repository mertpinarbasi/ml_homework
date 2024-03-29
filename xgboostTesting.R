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
  
  
  dmy <- dummyVars(" ~ .", data = df_income, fullRank = T)
  df_income <- data.frame(predict(dmy, newdata = df_income))
  
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
  
  
  
  best_param = list()
  best_seednumber = 1234
  best_logloss = Inf
  best_logloss_index = 0
  
  
  for (iter in 1:10) {
    
    param <-list(eta = 0.3 ,
                 max_depth= 6,
                 subsample=1,colsample_bytree=1
                 ,min_child_weight=1,
                 gamma=0,set.seed=best_seednumber
                 ,eval_metric="logloss",
                 eval_metric="error",
                 objective="binary:logistic",
                 booster="gbtree")
    
    cv.nround = 100
    cv.nfold = 10
    seed.number = sample.int(10000, 1)[[1]]
    set.seed(seed.number)
    mdcv <- xgb.cv(data= as.matrix( X_train), params = param, nthread=16,label = Y_train,
                   nfold=cv.nfold, nrounds=cv.nround,
                   verbose = T, early_stopping_rounds=8, maximize=FALSE)
    
  
    min_logloss_index = mdcv$best_iteration
    min_logloss = mdcv$evaluation_log[min_logloss_index]$test_logloss_mean
    
    if (min_logloss < best_logloss) {
      best_logloss = min_logloss
      best_logloss_index = min_logloss_index
      best_seednumber = seed.number
      best_param = param
    }
    
    
  }
  nround = best_logloss_index
  set.seed(best_seednumber)
  
  
  #run xgboost
  model <- xgboost(data= as.matrix( X_train),label = Y_train,nround=1000,params=best_param,verbose=1)
  
  #evaluate model 
  predictions = predict(model , newdata = as.matrix(X_test))
  predictions = ifelse(predictions>0.5,1,0)
  
  #check accuracy
  confusionMatrix(table(predictions,as.matrix(Y_test)))


