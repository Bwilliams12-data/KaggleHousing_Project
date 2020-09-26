library(dplyr)
library(glmnet)
library(Metrics)
library(caret)
library(e1071)
set.seed(123)
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)


###### Cleaning For Baseline Model (Umodified as Possible) #####

#Cleaning Train

train <- train[,-1]
train$LotFrontage[is.na(train$LotFrontage)] <- as.integer(mean(train$LotFrontage, na.rm = T))
train$Alley[is.na(train$Alley)] <- "No Alley"
train$MasVnrType[is.na(train$MasVnrType)] <- "None"
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$BsmtQual[is.na(train$BsmtQual)] <- "No Basement"
train$BsmtCond[is.na(train$BsmtCond)] <- "No Basement"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "No Basement"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "No Basement"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "No Basement"
train$Electrical[is.na(train$Electrical)] <- "SBrkr" #replaced with most common electrical
train$FireplaceQu[is.na(train$FireplaceQu)] <- "No Fireplace"
train$GarageType[is.na(train$GarageType)] <- "No Garage"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- as.integer(mean(train$GarageYrBlt, na.rm = T))
train$GarageFinish[is.na(train$GarageFinish)] <- "No Garage"
train$GarageQual[is.na(train$GarageQual)] <- "No Garage"
train$GarageCond[is.na(train$GarageCond)] <- "No Garage"
train$PoolQC[is.na(train$PoolQC)] <- "No Pool"
train$Fence[is.na(train$Fence)] <- "No Fence"
train$MiscFeature[is.na(train$MiscFeature)] <- "None"
train <- train[,-9] #Utilities only has one different observation so it should be removed
train$KitchenQual[956] <- "No Kitchen" #No kitchen so no rating
train$YearBuilt <- as.integer(train$YearBuilt)
train$Exterior1st <- as.factor(train$Exterior1st)
train$ExterCond <- as.factor(train$ExterCond)
train$GarageType <- as.factor(train$GarageType)
train$GarageFinish <- as.factor(train$GarageFinish)
train$GarageQual <- as.factor(train$GarageQual)
train$GarageCond <- as.factor(train$GarageCond)
train$GarageYrBlt[is.na(train$GarageYrBlt)]




#Cleaning Test

test <- test[,-1]
test$MSZoning[is.na(test$MSZoning)] <- "RL" #I decided to make these NA values RL because RL made up over 75% of the dataset so the likelihood of more than one of these being something other than RL was incredibly low
test$LotFrontage[is.na(test$LotFrontage)] <- as.integer(mean(test$LotFrontage, na.rm = T))
test$Alley[is.na(test$Alley)] <- "No Alley"
test <- test[,-9]
test$Exterior1st[is.na(test$Exterior1st)] <- "Plywood" #I went through the data and found that in every case where the roof style was flat and the material was tar and gravel the exterior was always made with plywood
test$Exterior2nd[is.na(test$Exterior2nd)] <- "Plywood" #plywood was the most common in conjunction with plywood
test$MasVnrType[is.na(test$MasVnrType)] <- "None"
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$BsmtQual[is.na(test$BsmtQual)] <- "No Basement"
test$BsmtCond[is.na(test$BsmtCond)] <- "No Basement"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "No Basement"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "No Basement"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "No Basement"
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0
test$KitchenQual[c(1129,1401)] <- "No Kitchen"
test$KitchenQual[is.na(test$KitchenQual)] <- "TA" #gave the NA kitchen and average rating, sort of like imputing the mean
test$Functional[is.na(test$Functional)] <- "Typ" #typ was the overwhelmingly most common variable
test$FireplaceQu[is.na(test$FireplaceQu)] <- "No Fireplace"
test$GarageType[is.na(test$GarageType)] <- "No Garage"
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- as.integer(mean(test$GarageYrBlt, na.rm = T))
test$GarageFinish[is.na(test$GarageFinish)] <- "No Garage"
test$GarageQual[is.na(test$GarageQual)] <- "No Garage"
test$GarageCond[is.na(test$GarageCond)] <- "No Garage"
test$GarageCars[is.na(test$GarageCars)] <- 0
test$GarageArea[is.na(test$GarageArea)] <- 0
test$PoolQC[is.na(test$PoolQC)] <- "No Pool"
test$Fence[is.na(test$Fence)] <- "No Fence"
test$MiscFeature[is.na(test$MiscFeature)] <- "None"
test$YearBuilt <- as.integer(test$YearBuilt)
test$Exterior1st <- as.factor(test$Exterior1st)
levels(test$Exterior1st) <- c(levels(test$Exterior1st),"ImStucc","Stone")
test$ExterCond <- as.factor(test$ExterCond)
test$GarageType <- as.factor(test$GarageType)
test$GarageFinish <- as.factor(test$GarageFinish)
test$GarageQual <- as.factor(test$GarageQual)
test$GarageCond <- as.factor(test$GarageCond)
test$SaleType[is.na(test$SaleType)] <- "WD" #Most common by far

##### Basline Regression (Unmodified as Possible)
ols_baseline <- lm(SalePrice~. , train)

fitted_ols <- predict(ols_baseline, test)#Score 0.47079

##### Further Cleaning #####

train$OverallQual <- as.character(train$OverallQual)
test$OverallQual <- as.character(test$OverallQual)
train$OverallCond <- as.character(train$OverallCond)   
test$OverallCond <- as.character(test$OverallCond)
train$BsmtFullBath <- as.character(train$BsmtFullBath)
test$BsmtFullBath <- as.character(test$BsmtFullBath)
train$BsmtHalfBath <- as.character(train$BsmtHalfBath)
test$BsmtHalfBath <- as.character(test$BsmtHalfBath)
train$HalfBath <- as.character(train$HalfBath)
test$HalfBath <- as.character(test$HalfBath)
train$BedroomAbvGr <- as.character(train$BedroomAbvGr)
test$BedroomAbvGr <- as.character(test$BedroomAbvGr)
train$KitchenAbvGr <- as.character(train$KitchenAbvGr)
test$KitchenAbvGr <- as.character(test$KitchenAbvGr)
train$MoSold <- as.character(train$MoSold)
test$MoSold <- as.character(test$MoSold)
train$MasVnrType[c(689,1242)] <- "None" #If area = 0 then there should be no Veneer
train <- train[,-c(33,35)] #redundant
test <- test[,-c(33,35)]

train$LotFrontage<-as.numeric(train$LotFrontage)#From Dr. Jorge Colazo
train$LotArea<-as.numeric(train$LotArea)
train$OverallQual <-as.numeric(train$OverallQual)
train$OverallCond <-as.numeric(train$OverallCond)
train$YearBuilt <-as.numeric(train$YearBuilt)
train$YearRemodAdd <-as.numeric(train$YearRemodAdd)
train$MasVnrArea <-as.numeric(train$MasVnrArea)
train$ExterCond <-as.numeric(train$ExterCond)
train$GarageYrBlt<-as.numeric(train$GarageYrBlt)

test$LotFrontage<-as.numeric(test$LotFrontage)
test$LotArea<-as.numeric(test$LotArea)
test$OverallQual <-as.numeric(test$OverallQual)
test$OverallCond <-as.numeric(test$OverallCond)
test$YearBuilt <-as.numeric(test$YearBuilt)
test$YearRemodAdd <-as.numeric(test$YearRemodAdd)
test$MasVnrArea <-as.numeric(test$MasVnrArea)
test$ExterCond <-as.numeric(test$ExterCond)
test$GarageYrBlt<-as.numeric(test$GarageYrBlt)

train <- train %>%
  select(-c(Street,Alley,LotShape,Condition2,RoofStyle,Exterior2nd,MasVnrType,MasVnrArea,OverallQual,ExterQual,BsmtFinType2,Electrical,X1stFlrSF,X2ndFlrSF,
            GarageYrBlt,GarageQual,PoolQC,MiscFeature,SaleType))

test <- test %>%
  select(-c(Street,Alley,LotShape,Condition2,RoofStyle,Exterior2nd,MasVnrType,MasVnrArea,OverallQual,ExterQual,BsmtFinType2,Electrical,X1stFlrSF,X2ndFlrSF,
            GarageYrBlt,GarageQual,PoolQC,MiscFeature,SaleType))

##### Clean Data OLS #####
ols_cleaned <- lm(SalePrice~. , train)

fitted_ols_clean <- predict(ols_cleaned, test)#Score 0.17405

##### Feature Selection #####

library(MASS)
#backwards
backwards <- stepAIC(ols_cleaned, direction="backward", na.action=na.remove)

fitted_backwards <- predict(backwards, test)#Score 0.15531

#forwards
forwards <- stepAIC(ols_cleaned, direction="forward", na.action=na.remove)

fitted_forwards <- predict(forwards, test)#Score 0.17405

#hybrid
hybrid <- stepAIC(ols_cleaned, direction="both", na.action=na.remove)

fitted_hybrid <- predict(hybrid, test)#Score 0.15531

##### Ridge Regression #####

lambda <- train(SalePrice~., method = "glmnet", data = train,na.action = na.exclude,tuneGrid = expand.grid(alpha = 0,lambda = seq(0.001,1000,by = 1)))

ridge <- predict(lambda,test)#Score 0.14639

##### LASSO Regression #####

lambda_lasso <- train(SalePrice~., method = "glmnet", data = train,na.action = na.exclude,tuneGrid = expand.grid(alpha = 1,lambda = seq(0.001,1000,by = 1)))

lasso <- predict(lambda_lasso,test)#Score 0.14638

##### SVM #####

#polynomial kernel

i <- sapply(train, is.character)
train[i] <- lapply(train[i], as.factor)

i <- sapply(test, is.character)
test[i] <- lapply(test[i], as.factor)
levels(test$HouseStyle) <- levels(train$HouseStyle)
levels(test$RoofMatl) <- levels(train$RoofMatl)
levels(test$Heating) <- levels(train$Heating)
levels(test$BedroomAbvGr) <- levels(train$BedroomAbvGr)
levels(test$KitchenAbvGr) <- levels(train$KitchenAbvGr)#Had to make all of my factor levels match up to get it to run

svm_poly <- svm(SalePrice~.,train,type = "eps-regression", cost = 0.1, kernel = "polynomial", scale = T)

poly_kernal <- predict(svm_poly,test)#Score 0.32288

#radial kernel
svm_radial  <- svm(SalePrice~.,train,type = "eps-regression", cost = 0.1, kernel = "radial", scale = T)

radial_kernal <- predict(svm_radial,test)#Score 0.17727

#linear kernel
svm_linear <- svm(SalePrice~.,train,type = "eps-regression", cost = 0.1, kernel = "linear", scale = T)

linear_kernel <- predict(svm_linear,test)#Score 0.14039

##### Random Forests #####

library(tree)
library(randomForest) 
library(gbm)

#bagging

bagging <- randomForest(SalePrice~.,mtry=57,train,ntrees=1000)#Adapted from Dr. Jorge Colazo        

bagging_57 <- predict(bagging,mtry=57,newdata=test)#Score 0.16263

#bagging with random variables

bagging2 <- randomForest(SalePrice~.,train,mtry=26,ntrees=1000)

bagging_26 <- predict(bagging2,mtry = 26,newdata = test)#Score 0.16057

#bagging with importance

bagging3 <- randomForest(SalePrice~.,mtry=26,train,ntrees = 1000, importance = T)

bagging_importance <- predict(bagging3,newdata = test)#Score 0.16063

#boosted 

boosted_model <- gbm(SalePrice~.,data=train,distribution="gaussian",n.trees=500)
boosted_results <- predict.gbm(boosted_model,newdata = test,n.trees = 500)#Score 0.19719

                               