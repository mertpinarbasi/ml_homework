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
train = subset(df_income, sample == TRUE)
test  = subset(df_income, sample == FALSE)

# one hot encoding for train set 
dmy <- dummyVars(" ~ .", data = train, fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train))

# one hot encoding for test set 
dmy_test <- dummyVars(" ~ .", data = test, fullRank = T)
test_transformed <- data.frame(predict(dmy, newdata = test))


