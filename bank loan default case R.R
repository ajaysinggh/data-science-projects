rm(list=ls(all=T))
setwd("C:\\Users\\Vijay\\Downloads")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
train_df = read.csv("bank-loan.csv", header = T, na.strings = c(" ", "", "NA"))
# Summarizing  data 

#Verify first five rows of data
head(train_df)
#target variable is 'defaut' and other variables are independent  variable(or predictors)
#Divide the data in test and train.
train_df1 =data.frame( c(train_df[1:700,1:9]))
train_df1
test_df1 =data.frame( c(train_df[701:851,1:9]))

#Verify  summary of data
summary(train_df)

#structure of  data
str(train_df)

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(train_df1,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train_df1)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val




 ############################################Outlier Analysis#############################################
 # ## BoxPlots - Distribution and Outlier Check
 numeric_index = sapply(train_df1,is.numeric) #selecting only numeric
 
 numeric_data = train_df1[,numeric_index]
 
 cnames = colnames(numeric_data)
 
  for (i in 1:length(cnames))
  {
    assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = train_df1$default), data = subset(train_df1))+ 
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                          outlier.size=1, notch=FALSE) +
             theme(legend.position="bottom")+
             labs(y=cnames[i],x=train_df1$default)+
             ggtitle(paste("Box plot of responded for",cnames[i]))) }
 # 
 ## Plotting plots together
  gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
  gridExtra::grid.arrange(gn4,gn5,ncol=2)
  gridExtra::grid.arrange(gn6,gn7,ncol=2)
  gridExtra::grid.arrange(gn8,ncol=1)
 # 
  # #Remove outliers using boxplot method
  #there are outliers in income
  val = train_df1$income[train_df1$income %in% boxplot.stats(train_df1$income)$out]
  
  train_df1 = train_df1[which(!train_df1$income %in% val),]

   # Boxplot after removing  outliers
  
  # boxplot for  income variable
  
  ggplot(data = train_df1, aes(x = "", y = income)) + 
    geom_boxplot() 
  #there are outliers in address
  val = train_df1$address[train_df1$address %in% boxplot.stats(train_df1$address)$out]
  
  train_df1 = train_df1[which(!train_df1$address %in% val),]
  # Boxplot after removing  outliers
  
  # boxplot for  address variable
  
  ggplot(data = train_df1, aes(x = "", y = address)) + 
    geom_boxplot() 
  
  #there are outliers in employ
  val = train_df1$employ[train_df1$employ %in% boxplot.stats(train_df1$employ)$out]
  
  train_df1 = train_df1[which(!train_df1$employ %in% val),]
  # Boxplot after removing  outliers
  
  # boxplot for  employ variable
  
  ggplot(data = train_df1, aes(x = "", y = employ)) + 
    geom_boxplot() 
  
  
 # Analyze variables  by visualize
# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(train_df1)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}


# analyze the distribution of  target variable 'default'
univariate_numeric(train_df1$default)

# analyse the distrubution of  independence variable 'age'
univariate_numeric(train_df1$age)

# analyse the distrubution of  independence variable 'ed'
univariate_numeric(train_df1$ed)

# analyse the distrubution of  independence variable 'employ'
univariate_numeric(train_df1$employ)

# analyse the distrubution of  independence variable 'address'
univariate_numeric(train_df1$address)

# analyse the distrubution of  independence variable 'income'
univariate_numeric(train_df1$income)

# analyse the distrubution of  independence variable 'debtinc'
univariate_numeric(train_df1$debtinc)

# analyse the distrubution of  independence variable 'creddebt'
univariate_numeric(train_df1$creddebt)

# analyse the distrubution of  independence variable 'othdebt'
univariate_numeric(train_df1$othdebt)

##################################Feature Selection################################################
## Correlation Plot 
corrgram(train_df1[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(train_df1$default, p = .80, list = FALSE)
train =data.frame( train_df1[ train.index,])
test = data.frame( train_df1[-train.index,])
train_df1$default = as.factor(train_df1$default)
train$default = as.factor(train$default)
str(train_df1)
structure(train_df1)
##Decision tree for classification
#Develop Model on training data

C50_model = C5.0(default ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")


#Lets predict for test cases
C50_Predictions = predict(C50_model, test, type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$default, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
FNR = FN/FN+TP  
FNR= 76.46
#ACCURACY =  0.7222

#ROC CURVE FOR DECISION TREE
library(ROCR)
library(pROC)
pred1 = predict(C50_model,test,type = 'prob')

pred = prediction(pred,train$default)
plot(roc(test$default,pred1[,2]),
     colorize = T,
     main = 'ROC CURVE',
     ylab = 'sensitivity',
     xlab = ' 1-sensitivity')

     


###Random Forest
RF_model = randomForest(default ~ ., train, importance = TRUE, ntree = 500)
#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test)

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$default, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
confusionMatrix
#False Negative rate
FNR = FN/FN+TP 
FNR = 70.58
#accuracy = 0.7381 
library(pROC)
pred2 = predict(RF_model,test,type = 'prob')

plot(roc(test$default,pred2[,2]),
     colorize = T,
     main = 'ROC CURVE',
     ylab = 'sensitivity',
     xlab = ' 1-sensitivity')


#Logistic Regression
logit_model = glm(default ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
logit_Predictions
#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(test$default, logit_Predictions)
ConfMatrix_RF
#Accuracy ((TP+TN)/total observation))
#accuracy = 78.57
#False Negative rate
#FNR = FN/FN+TP= 59.45
#RoC curve FOR LOGISTIC REGRESSION
library(nnet)
mymodel = multinom(default ~ ., data = train )
library(ROCR)
pred = predict(mymodel,test,type = 'prob')
pred = prediction(pred,train$default)

roc1 = performance(pred,'tpr','fpr')
plot(roc,
     colorize = T,
     main = 'ROC CURVE',
     ylab = 'sensitivity',
     xlab = ' 1-sensitivity')
abline(a=0,b=1)


