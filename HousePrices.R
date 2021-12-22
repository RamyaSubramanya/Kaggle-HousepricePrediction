setwd(dir = "D:\\IIM - K\\SUPERVISED MACHINE LEARNING\\REGRESSION\\REG-PRACTICALS\\HousePrices(Kaggle)")

train<-read.csv("train.csv")

test<-read.csv("test.csv")

install.packages("dplyr",dependencies = TRUE)
library(dplyr)

--------------------------------------------------------------------------------
#step1: Combining 2 datasets using bind_rows() to perform data pre-processing
?bind_rows

data1<-bind_rows(train,test)

sum(is.na(data1))
colSums(is.na(data1))

data2<-data1[,-c(1,2,20,21,60,77,78)]

data2<-data2[,-c(16,17,24,25,27,28,29,30,32,37,50,54,59,60,68,69)]

str(data2)

colSums(is.na(data2))

#Since Alley column has 80% missing values we can remove the column as it will not help our prediction.

data2<-data2[,-c(5,54)]


--------------------------------------------------------------------------------
#Step2: Handling Missing Values using Mean, Mode approach
  
#MSZoning

table(data2$MSZoning)

data2$MSZoning[which(is.na(data2$MSZoning))]<-mode(data2$MSZoning)

data2$MSZoning[data2$MSZoning=="character"]<-"RL"

#LotFrontage

data2$LotFrontage[which(is.na(data2$LotFrontage))]<-mean(data2$LotFrontage,na.rm = TRUE)


#Utilities

table(data2$Utilities)

data2$Utilities[which(is.na(data2$Utilities))]<-mode(data2$Utilities)
data2$Utilities[data2$Utilities=="character"]<-"AllPub"


#Exterior1st

table(data2$Exterior1st)

data2$Exterior1st[which(is.na(data2$Exterior1st))]<-mode(data2$Exterior1st)
data2$Exterior1st[data2$Exterior1st=="character"]<-"Vinylsd"


#Exterior2nd

table(data2$Exterior2nd)

data2$Exterior2nd[which(is.na(data2$Exterior2nd))]<-mode(data2$Exterior2nd)
data2$Exterior2nd[data2$Exterior2nd=="character"]<-"Vinylsd"

#MasVnrType

table(data2$MasVnrType)

data2$MasVnrType[which(is.na(data2$MasVnrType))]<-mode(data2$MasVnrType)
data2$MasVnrType[data2$MasVnrType=="character"]<-"None"


#MasVnrArea

data2$MasVnrArea[which(is.na(data2$MasVnrArea))]<-mean(data2$MasVnrArea,na.rm = TRUE)

colSums(is.na(data2))

#BsmtFinSF1,F2,SF

data2$BsmtFinSF1[which(is.na(data2$BsmtFinSF1))]<-mean(data2$BsmtFinSF1,na.rm = TRUE)

data2$BsmtFinSF2[which(is.na(data2$BsmtFinSF2))]<-mean(data2$BsmtFinSF2,na.rm = TRUE)

data2$BsmtUnfSF[which(is.na(data2$BsmtUnfSF))]<-mean(data2$BsmtUnfSF,na.rm = TRUE)


#TotalBsmtSF

data2$TotalBsmtSF[which(is.na(data2$TotalBsmtSF))]<-mean(data2$TotalBsmtSF,na.rm = TRUE)

table(data2$Electrical)
table(data2$BsmtFullBath)
table(data2$BsmtHalfBath)

data2$Electrical[which(is.na(data2$Electrical))]<-mode(data2$Electrical)
data2$BsmtFullBath[which(is.na(data2$BsmtFullBath))]<-mode(data2$BsmtFullBath)
data2$BsmtHalfBath[which(is.na(data2$BsmtHalfBath))]<-mode(data2$BsmtHalfBath)

data2$Electrical[data2$Electrical=="character"]<-"SBrkr"
data2$BsmtFullBath[data2$BsmtFullBath=="numeric"]<-0
data2$BsmtHalfBath[data2$BsmtHalfBath=="numeric"]<-0

colSums(is.na(data2))

table(data2$Functional)
data2$Functional[which(is.na(data2$Functional))]<-mode(data2$Functional)
data2$Functional[data2$Functional=="character"]<-"Typ"

table(data2$GarageType)
data2$GarageType[which(is.na(data2$GarageType))]<-mode(data2$GarageType)
data2$GarageType[data2$GarageType=="character"]<-"Attchd"

table(data2$GarageFinish)
data2$GarageFinish[which(is.na(data2$GarageFinish))]<-mode(data2$GarageFinish)
data2$GarageFinish[data2$GarageFinish=="character"]<-"Unf"

table(data2$GarageCars)
data2$GarageCars[which(is.na(data2$GarageCars))]<-mode(data2$GarageCars)
data2$GarageCars[data2$GarageCars=="numeric"]<-2

data2$GarageArea[which(is.na(data2$GarageArea))]<-mean(data2$GarageArea,na.rm = TRUE)

table(data2$SaleType)

data2$SaleType[which(is.na(data2$SaleType))]<-mode(data2$SaleType)
data2$SaleType[data2$SaleType=="character"]<-"WD"

colSums(is.na(data2))

sum(is.na(data2))
--------------------------------------------------------------------------------
#Step3: Creating Dummy Variables for Nominal & Categorical data

colnames(data2)
str(data2)

dummy<-data2

install.packages("fastDummies", dependencies = TRUE)
library(fastDummies)

?dummy_cols

dummy<-dummy_cols(dummy,select_columns = "MSZoning")
dummy<-dummy_cols(dummy,select_columns = "Street")
dummy<-dummy_cols(dummy,select_columns = "LotShape")
dummy<-dummy_cols(dummy,select_columns = "LandContour")
dummy<-dummy_cols(dummy,select_columns = "Utilities")
dummy<-dummy_cols(dummy,select_columns = "LotConfig")
dummy<-dummy_cols(dummy,select_columns = "LandSlope")
dummy<-dummy_cols(dummy,select_columns = "Neighborhood")
dummy<-dummy_cols(dummy,select_columns = "Condition1")
dummy<-dummy_cols(dummy,select_columns = "Condition2")
dummy<-dummy_cols(dummy,select_columns = "BldgType")
dummy<-dummy_cols(dummy,select_columns = "HouseStyle")
dummy<-dummy_cols(dummy,select_columns = "RoofStyle")
dummy<-dummy_cols(dummy,select_columns = "RoofMatl")
dummy<-dummy_cols(dummy,select_columns = "Exterior1st")
dummy<-dummy_cols(dummy,select_columns = "Exterior2nd")
dummy<-dummy_cols(dummy,select_columns = "MasVnrType")
dummy<-dummy_cols(dummy,select_columns = "Foundation")
dummy<-dummy_cols(dummy,select_columns = "BsmtQual")
dummy<-dummy_cols(dummy,select_columns = "BsmtCond")
dummy<-dummy_cols(dummy,select_columns = "BsmtExposure")
dummy<-dummy_cols(dummy,select_columns = "BsmtFinType1")
dummy<-dummy_cols(dummy,select_columns = "BsmtFinType2")
dummy<-dummy_cols(dummy,select_columns = "Heating")
dummy<-dummy_cols(dummy,select_columns = "CentralAir")
dummy<-dummy_cols(dummy,select_columns = "Electrical")
dummy<-dummy_cols(dummy,select_columns = "BsmtFullBath")
dummy<-dummy_cols(dummy,select_columns = "BsmtHalfBath")
dummy<-dummy_cols(dummy,select_columns = "Functional")
dummy<-dummy_cols(dummy,select_columns = "GarageType")
dummy<-dummy_cols(dummy,select_columns = "GarageFinish")
dummy<-dummy_cols(dummy,select_columns = "GarageCars")
dummy<-dummy_cols(dummy,select_columns = "PavedDrive")
dummy<-dummy_cols(dummy,select_columns = "SaleType")
dummy<-dummy_cols(dummy,select_columns = "SaleCondition")


table(data2$MSZoning)
table(data2$Street)
table(data2$LotShape)
table(data2$LandContour)
table(data2$Utilities)
table(data2$LotConfig)
table(data2$LandSlope)
table(data2$Neighborhood)
table(data2$Condition1)
table(data2$Condition2)
table(data2$HouseStyle)
table(data2$RoofStyle)
table(data2$RoofMatl)
table(data2$Exterior1st)
table(data2$Exterior2nd)
table(data2$MasVnrType)

table(data2$Foundation)

table(data2$Heating)
table(data2$CentralAir)
table(data2$Electrical)
table(data2$BsmtFullBath)
table(data2$BsmtHalfBath)
table(data2$Functional)
table(data2$GarageType)
table(data2$GarageFinish)
table(data2$GarageCars)
table(data2$PavedDrive)
table(data2$SaleType)
table(data2$SaleCondition)

str(dummy)
dummy<-dummy[,-c(1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,26,27,28,33,34,40,42,43,44,46,54,55)]

str(dummy)

dummy<-dummy[,-c(27,32,36,40,43,47,51,53,84,91,102,112,113,133,146,154,163,164,170,175,180,183,189,195,197,205,207,210,219)]

str(dummy)

dummy<-dummy[,-c(85)]
--------------------------------------------------------------------------------
#Step4: Splitting the dataframe into Training & Test data
  
?split

data3<-dummy

Traindata<-data3[1:1460,]
Testdata<-data3[1461:2919,]

#####
install.packages("caTools",dependencies = TRUE)
library(caTools)

split<-sample.split(Traindata,SplitRatio = 0.70)
split

FinalTrain<-subset(Traindata,split=TRUE)
FinalTest<-subset(Traindata,split=FALSE)

####
--------------------------------------------------------------------------------
#Step5: Linear Regression
  
  
install.packages("caret",dependencies = TRUE)
library(caret)
  
install.packages("glmnet",dependencies = TRUE)
library(glmnet)
  
install.packages("psych",dependencies = TRUE)
library(psych)

?trainControl
custom<-trainControl(method = "repeatedcv", number = 10, repeats = 5)
  
?train
linear<-train(SalePrice~.,data=Traindata,method="lm",trControl=custom)

linear$results
summary(linear)
plot(varImp(linear,scale = TRUE))

------------------------------------------------------------------------------------

#Ridge 

ridge<-train(SalePrice~.,data=Traindata,method="glmnet",
             tuneGrid=expand.grid(alpha=0,lambda=seq(0.001,1,length=5)), trControl=custom)

ridge$results
ridge
summary(ridge)

plot(varImp(ridge,scale = TRUE))
  
--------------------------------------------------------------------------------
#Lasso
  
lasso<-train(SalePrice~.,data=Traindata,method="glmnet",
               tuneGrid=expand.grid(alpha=1,lambda=seq(0.001,1,length=5)), trControl=custom)

lasso$results
lasso
summary(lasso)

plot(varImp(lasso,scale = TRUE))

---------------------------------------------------------------------------------------
#compare

modellist<-list(linear=linear,ridge=ridge,lasso=lasso)
compare<-resamples(modellist)
summary(compare)

#Predict

install.packages("Metrics",dependencies = TRUE)
library(Metrics)

Predicted<-predict(ridge, Testdata)
Predicted

write.csv(Predicted,"Predicted.csv")


Finals<-data.frame(Actuals=Traindata$SalePrice,Predicted=Predicted)

RMSE<-rmse(Finals$Actuals,Finals$Predicted)
RMSE

#Our RMSE is 27.39% which is pretty good and here Ridge Regression technique is preferred as RMSE is low in Ridge compared to other models.
