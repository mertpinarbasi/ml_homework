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
sample = sample.split(df_income$income, SplitRatio = .75)
trainingSet = subset(df_income, sample == TRUE)
testSet  = subset(df_income, sample == TRUE)

# isolaate y cariable 
Y_train <- trainingSet$income
Y_test <- testSet$income

#isolate x cariable 

X_test <- subset(testSet,select=-c(income))



# one hot encoding for train set 

dmyAllSet <- dummyVars(" ~ .", data = trainingSet, fullRank = T)
trainingSet <-  data.frame(predict(dmyAllSet, newdata = trainingSet))


dmyTest <- dummyVars(" ~ .", data = X_test, fullRank = T)
X_test <- data.frame(predict(dmyTest, newdata = X_test))


# evalution of svm 
library(e1071)
svm_classifier = svm(formula=income ~ .,data=trainingSet,type="C-classification",kernel="radial",scale=TRUE,cost=10)
Y_pred = predict(svm_classifier,newdata= X_test)
confusionMatrix(table(Y_test,Y_pred))



# cross validation 

# in creating the folds we specify the target feature (dependent variable) and # of folds
folds = createFolds(trainingSet$income, k = 10)
# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = trainingSet[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = trainingSet[x, ] # here we describe the test fold individually
  # now apply (train) the classifer on the training_fold
  classifier = svm_classifier
  Y_pred = predict(svm_classifier,newdata= test_fold[-97])
  cm = table(test_fold[, 97], Y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
  
  })
accuracy = mean(as.numeric(cv))
accuracy

# Visualising the Training set results

plot(svm_classifier,trainingSet,age~hours.per.week)