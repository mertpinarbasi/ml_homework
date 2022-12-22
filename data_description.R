library(caret)
library(pROC)
library(ggplot2)

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


# one hot encoding 

dmy <- dummyVars(" ~ .", data = df_income, fullRank = T)
df_income <- data.frame(predict(dmy, newdata = df_income))





## corellation matrix 


library(reshape2)

cor_mat <- cor(df_income[,c("income","age","education.num","hours.per.week",
                            "capital.gain","capital.loss")])
cor_mat [lower.tri(cor_mat)] <- NA
melted_cormat <- melt(cor_mat,na.rm = TRUE)
#heatmap
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 15, vjust = 1, 
                                   size = 15, hjust = 1))+
  theme(axis.text.y = element_text( vjust = 1, 
                                    size = 15, hjust = 1))+
  coord_fixed()+ 
  ggtitle("Continous Values\nCorrelation Heatmap")+
  labs(x="",y="")
coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1,label = round(value, digits = 4)), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
   title.position = "top", title.hjust = 0.5))

#income distribution

library(dplyr)
df_income$income <-as.factor(df_income$income)
df_income %>% ggplot(aes(education.num, hours.per.week)) +
  (geom_point(aes(col=income))) + 
  scale_color_manual(values = c("1" = "springgreen3", "0" = "tomato"))+

  ggtitle("Income Distribution depending on years of education and working hours per week") +
  labs(x="Years of education",y="Hours per Week")


ggplot(df_income, aes(x=income,fill=income)) + geom_bar() +
  scale_x_discrete(labels=c("<=50K", ">50K"))+
  scale_fill_manual(values=c("tomato", "springgreen3")) +
  labs(x="income", y="count")


# age corelattion

ggplot(df_income) + aes(x=age, group=income,fill=income) + 
  geom_histogram( binwidth=1, color='black')+
  scale_fill_manual(values=c("tomato", "springgreen3")) +
  labs(x="Age",y="Count")+ 
  ggtitle("Income Distribution Correllation Between  Age")