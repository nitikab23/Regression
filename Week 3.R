library(mctest)
library(tidyr)
library(ggplot2)
library(caret)
library(rsq)
library(ncvreg)
library(bigmemory)
library(biglasso)
library(bigmemory.sri)
library(Matrix)
library(lars)
library(glmnet)
library(e1071)
library(ggcorrplot)
library(corrplot)
library(dummies)
library(dplyr)
library(polycor)
library(psych)
library(nlme)
library(randomForest)
library(survival)
library(recipes)
library(car)
library(xgboost)
library(stringr)
library(readr)

setwd("D:\\Me\\NEU\\Kasun - Data Mining\\Module 2\\")
train <- read.csv("train.csv", header = TRUE) 

################################
#####Data Exploration
################################

#checking str of data
str(train)
ncol(train)

#missing rows
missing_row <- train[!complete.cases(train),]
nrow(missing_row)

names(train)

#selecting important variables
train <- train[,c( "SalePrice" , "MSSubClass","MSZoning","LotArea","Street","Utilities","Neighborhood" ,   "BldgType"  ,  "OverallQual" ,  "OverallCond"  , "YearBuilt"    
                  ,"YearRemodAdd" ,  "ExterQual" , "ExterCond"   ,  "Foundation" ,   "BsmtQual"   ,   "BsmtCond" ,  "BsmtFinType1",  "BsmtFinSF1"  
              , "BsmtFinSF2" ,   "BsmtUnfSF"  , "Heating"  , "HeatingQC"  ,   "CentralAir" ,   "Electrical" ,  "LowQualFinSF" , "GrLivArea"    , "BsmtFullBath" , "BsmtHalfBath" , "FullBath",   "HalfBath"    ,  "BedroomAbvGr" 
                , "KitchenAbvGr" , "KitchenQual"  , "TotRmsAbvGrd" 
               , "Fireplaces"   , "FireplaceQu" ,  "GarageType" , "GarageCars"  ,"GarageQual"   
            , "GarageCond"  ,"WoodDeckSF" ,   "OpenPorchSF"  , "EnclosedPorch" , "ScreenPorch"  , "PoolArea"    , "PoolQC"    ,    "Fence" ,  "MiscVal"   ,   
      "MoSold" , "YrSold" )]

ncol(train)

#checking the data and n/a
str(train)
summary(train)

#Changing the categorical variables to ordered numeric variables
ordervar <- c("ExterCond","ExterQual","BsmtCond","BsmtQual","HeatingQC","KitchenQual","FireplaceQu","GarageQual","PoolQC","GarageCond")
train1 <- train[,ordervar]
for (i in 1:length(ordervar))
{train1[,i] <- as.numeric(factor(train1[,i], 
                                     levels = c("Ex", "Fa","Gd", "TA","Po"),
                                     labels = c(5,2,4,3,1) ,ordered = TRUE))}

for (i in 1:length(ordervar)){
(train1[,i][is.na(train1[,i])] <- 0)}

#changing multiple categorical variables to separate dummy columns
dummyvar <- c("MSZoning","Street","Utilities","Neighborhood","BldgType","Foundation","BsmtFinType1","Heating","Electrical","GarageType","Fence")
train2 <- train[,dummyvar]
for (i in 1:length(dummyvar))
{ a <- dummy(train2[,i], sep = ".")
a <-  data.frame(a) 
names(a) <- paste(dummyvar[i],names(a))
  train2 <- cbind(train2,a)}

#adding train1 and train2 columns to main data

main_train <- train[,-which(names(train) %in% ordervar)]
main_train <- main_train[,-which(names(main_train) %in% dummyvar)]
train2 <- train2[,-which(names(train2) %in% dummyvar)]
main_train <- data.frame(c(main_train,train1,train2))

#checking STR and summary
summary(main_train)
str(main_train)

#changing double categorical variable to dummy variable
main_train$CentralAir <- as.numeric(factor(main_train$CentralAir, 
                                      levels = c("Y","N"),
                                      labels = c(1,0)))

#checking the target variable
describe(main_train$SalePrice)
summary(main_train$SalePrice)

#Visualizing Target variable to check distribution
options(scipen=10000)
ggplot(main_train, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("SalePrice Distribution") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))

#Skewed Towards right so taking log
main_train$logSalePrice <- log(main_train$SalePrice)

#visualizing Log
ggplot(main_train, aes(x = logSalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 0.05) +
  ggtitle("Log SalePrice Distribution") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))



#Visualizing prices as by Overall Quality
ggplot(main_train, aes(x = SalePrice,fill = as.factor(OverallQual))) +
  geom_histogram(position = "stack", binwidth = 10000) +
  ggtitle("Histogram of SalePrice") +
  ylab("Count") +
  xlab("Housing Price") + 
  scale_fill_discrete(name="OverallQual")+
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.9,0.7), legend.background = element_rect(fill="grey90"))
 

main_train$SalePrice <- NULL

ggplot(aB, aes(x=SalePrice, fill=Type)) + geom_density(alpha=.3)+ ggtitle("Comparison of Houses based on Central Air")
    hghg
    
############################################Sampling
##############################################
set.seed(101)
sample <- sample.int(n = nrow(main_train), size = floor(.75*nrow(main_train)), replace = F)
main_train <- main_train[sample, ]
test  <- main_train[-sample, ]

#################################################
##################Modelling
#################################################

#Checking the correlation between Variables
names(main_train)
corr <- cor(main_train[,c(1:20,116)])
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Air-bnb Prices", 
           ggtheme=theme_bw)



###################################
#Linear Regression Model

#setting the linear model
mymodel <- glm(main_train$logSalePrice~.,data=main_train)
summary(mymodel)
plot(mymodel)
rsq(mymodel)


####################################################Week 3
###############################################################
##################################################################


####################Checking Multicol#################
X <- main_train[,c(1:115)]
Y <- main_train$logSalePrice
imcdiag(X,Y)

#removing unneccesary variables
main_train2 <- main_train[,c(2:4,6:11,13:25,27:36,38,40:116)]

#applying the linear model again
mymodel1 <- glm(main_train2$logSalePrice~.,data=main_train2)
summary(mymodel1)
rsq(mymodel1)

#Training Error and Accuracy
a <- predict(mymodel)
A <- RMSE(a,main_train$logSalePrice)

b <- predict(mymodel1)
B<- RMSE(b,main_train$logSalePrice)

#Predictions
MPred<- predict(mymodel,test)
M1Pred <- predict(mymodel1,test)

#MAE
MAE <- MAE(MPred,test$logSalePrice)
MAE1 <- MAE(M1Pred,test$logSalePrice)


#AIC, BIC

lm1 <- c(A,1-A, BIC(mymodel), AIC(mymodel), rsq(mymodel),RMSE(MPred,test$logSalePrice),MAE )
lm2 <- c(B,1-B, BIC(mymodel1), AIC(mymodel1), rsq(mymodel1),RMSE(M1Pred,test$logSalePrice), MAE1)


############################################
#Lasso Regularized Regression#

y <- main_train[,116]
x <- model.matrix(main_train$logSalePrice~.,data = main_train)


#regularizing the model
glm <- glmnet(x,y)
plot(glm,xvar = "norm",label = TRUE)


#cross validating the model
cv.glm <- cv.glmnet(x,y,alpha=1,nlambda=1000)
plot(cv.glm)


#fitting the model
fit <-  glmnet(x,y,alpha =1, lambda = cv.glm$lambda.min)

cv.glm$cvm
fit$beta

summary(fit)

#AIC, BIC for lasso
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc
AICcL <- AICc

BIC<-log(n)*k - tLL
BICL <- BIC

############################################
#Ridge Regularized Regression#


#cross validating the model
cv.glm <- cv.glmnet(x,y,alpha=0,nlambda=1000)
plot(cv.glm)


#fitting the model
fitR <-  glmnet(x,y,alpha =0, lambda = cv.glm$lambda.min)

cv.glm$cvm
fitR$beta

summary(fitR)

#AIC, BIC for lasso
tLL <- fitR$nulldev - deviance(fitR)
k <- fitR$df
n <- fitR$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc
AICcR <- AICc

BIC<-log(n)*k - tLL
BICR <- BIC


#Checking the fitting measures of all the four models

c <- predict(fit,newx = x)
C <- 1- RMSE(c,main_train$logSalePrice)

d <- predict(fitR,newx = x)
D <- 1-RMSE(d,main_train$logSalePrice)


##################LASSO
newx <- model.matrix(logSalePrice~.,data = test)
lassopredict <- predict(fit, newx = newx)
RMSELasso <- RMSE(lassopredict,test$logSalePrice)

#####################Ridge
ridgepredict <- predict(fitR, newx = newx)
RMSERidge <- RMSE(ridgepredict,test$logSalePrice)

###Performance of Lasso
Lasso <- c(C,1-C, BICL, AICcL,RMSELasso, MAE(lassopredict,test$logSalePrice))
Ridge <- c(D,(1-D), BICR, AICcR, RMSERidge, MAE(ridgepredict,test$logSalePrice))


##################Regression Model
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# FitnLinear Regressiob Model
model <- train(logSalePrice~., data=main_train, trControl=train_control, method="lm")
# Summarise Results
print(model)

##################################################
#Random Forest 

#Cross Validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(101)
tunegrid <- expand.grid(.mtry=c(1:50))
rf_sale <- train(logSalePrice~., data=main_train, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_sale)
plot(rf_sale)


#Setting the model
RFmodel <- randomForest(main_train$logSalePrice ~. , data = main_train, method = "anova", 
                        ntree = 100,
                        mtry = 26,
                        replace = F,
                        nodesize = 1,
                        importance = T)

imp <- importance(RFmodel)

importance_rfmodel <- data.frame(variable= row.names(imp), importance= round( imp[,"IncNodePurity"],2))

importance_rfmodel %>% top_n(importance,n=10) %>% 
  ggplot(aes(reorder(variable,importance),importance)) + geom_bar(stat= "identity") + 
  xlab("Features") + theme_minimal(base_family = "Ubuntu Condensed") + coord_flip() +
  theme(axis.text.x = element_blank()) + ylab("Importance")+
  ggtitle("Feature importance using randomforest")

#Predicting the values
RFmodelpred <- predict(RFmodel, test, type="response",
                       norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

#Comparison with regression coeffs
RFRMSE <- RMSE(RFmodelpred,test$logSalePrice)
RFMAE <- MAE(RFmodelpred,test$logSalePrice)

plotlm <- ggplot(test, aes(x = test$logSalePrice, y = MPred)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = test$logSalePrice, yend = MPred), alpha = .2) +
  # > Color adjustments made here...
  geom_point(aes(color = test$logSalePrice-MPred)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  # <
  geom_point(aes(y = MPred), shape = 1) +
  theme_bw()+
  ggtitle("Linear Model - Actual Vs Predicted")
plotlasso <- ggplot(test, aes(x = test$logSalePrice, y = lassopredict)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = test$logSalePrice, yend = lassopredict), alpha = .2) +
  # > Color adjustments made here...
  geom_point(aes(color = test$logSalePrice-lassopredict)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  # <
  geom_point(aes(y = lassopredict), shape = 1) +
  theme_bw()+
  ggtitle("Lasso Model - Actual Vs Predicted")
plotridge <- ggplot(test, aes(x = test$logSalePrice, y = ridgepredict)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = test$logSalePrice, yend = ridgepredict), alpha = .2) +
  # > Color adjustments made here...
  geom_point(aes(color = test$logSalePrice-ridgepredict)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  # <
  geom_point(aes(y = ridgepredict), shape = 1) +
  theme_bw()+
  ggtitle("Ridge Model - Actual Vs Predicted")
plotrf <- ggplot(test, aes(x = test$logSalePrice, y = RFmodelpred )) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = test$logSalePrice, yend = RFmodelpred), alpha = .2) +
  # > Color adjustments made here...
  geom_point(aes(color = test$logSalePrice-RFmodelpred)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  # <
  geom_point(aes(y = RFmodelpred), shape = 1) +
  theme_bw()+
  ggtitle("Random Forest - Actual Vs Predicted")

ggarrange(plotlm, plotlasso, plotridge, plotrf + rremove("x.text"))