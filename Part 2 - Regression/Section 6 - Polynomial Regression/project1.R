# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 
# install.packages("lattice")
# install.packages("ggplot2")
# install.packages("grDevices")
# install.packages("randomForest")
# install.packages("mice")
# install.packages("psych")
# install.packages("e1071")
# install.packages("sjPlot")
# 
# 
# 
# library(lattice)
# library(ggplot2)
# library(grDevices)
# library(randomForest)
# library(mice)
# library(psych)
# library(e1071)
# library(sjPlot)
# grDevices package has colorRamp and colorRampPalette functions
#require(leaps)
#R package leaps has a function regsubsets that can be used for best subsets

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Any results you write to the current directory are saved as output.
# ROOT.DIR <- ".." #getwd()
# DATA.DIR <- paste(ROOT.DIR,"input",sep="/")
HouseData <- read.csv(file.path("train.csv"), header = TRUE, sep = ",", na.strings = "NA")
TestData <- read.csv(file.path("test.csv"), header = TRUE, sep = ",", na.strings = "NA")
submission <- read.csv(file.path("sample_submission.csv"), header = TRUE, sep = ",", na.strings = "NA")

summary(HouseData)
summary(TestData)

describe(HouseData) # gives mean, sd, median, skewness and kurtosis for all variables
sjp.corr(HouseData[, sapply(HouseData, is.numeric)])

print("Train data dimensions")
nrow(HouseData)
ncol(HouseData)
print("Test data dimensions")
nrow(TestData)
ncol(TestData)

#-------------------------------------------------------------------------------
print("Graphs")
# price per sft
Pricepersft <- (HouseData$GrLivArea)/(HouseData$SalePrice)

#Lot Area
hist(HouseData$LotArea, breaks = 40, xlab = 'LotArea',
     main = paste("Histogram of" , "Lot Area"), col = 'pink4')

bins_lotarea <- c(0, 2500, 5000, 7500, 10000, 12500, 15000, 20000, 25000, 30000, 215245)
hist(HouseData$LotArea, breaks = bins_lotarea, 
     xlab = "Histogram of lot area", xlim = c(0, 30000), #las=2,
     col = 'Blue')

#Lot Area in k
#bins_lotarea_K <- c(0, 250, 500, 750, 1000, 1250, 1500, 2000, 2500, 3000, 22000)
#hist(HouseData$LotArea_K, breaks = bins_lotarea_K, xlab = "Histogram of lot area", xlim = c(0, 3000), 
#las=2,col = 'Blue')

#Sale Price
bins_saleprice_k <- c(0, 50, 100, 150, 200, 300, 400, 500, 600, 800)
hist(HouseData$SalePrice, breaks = 15,
     xlab = "Histogram of sale price in k", col = 'purple')

# histogram using qplot
qplot(HouseData$SalePrice, geom = "histogram", binwidth = 20, 
      col=I("blue"), fill=I("orange"))

qplot(HouseData$LotArea, HouseData$SalePrice, geom = "point", data = HouseData,
      color = BldgType, xlim = range(1, 30000), 
      xlab = 'Lot Area', ylab = 'Sale price', main = 'sale price by lot area')
# price by foundation
plot(HouseData$Foundation, HouseData$SalePrice, col = 'yellow', 
     xlab = 'Foundation', ylab = 'Sale price', main = 'sale price by foundation type')
#year built 
hist(HouseData$YearBuilt, breaks = 40, col = 'Red',
     xlab = "Histogram of Year Built", xlim = range(1872, 2010) )
# panel plots
xyplot(HouseData$SalePrice ~ HouseData$YearBuilt, data = HouseData, 
       xlab = "Year Built", ylab = "Sale Price", main = "sale price by year built",
       panel = function(YearBuilt, SalePrice)
       { panel.xyplot(HouseData$YearBuilt, HouseData$SalePrice, col = 3)
         panel.lmline(HouseData$YearBuilt, HouseData$SalePrice, col = 2, lty = 3)
         #panel.abline(h=median(housedata$saleprice_K), lty = 2)
       })
cor(HouseData$YearBuilt, HouseData$SalePrice)
#Bldg Type
bwplot(HouseData$SalePrice ~ HouseData$BldgType, xlab = 'Building Type', 
       ylab = 'Sale', main = 'sale price by building type')
plot(HouseData$MoSold, HouseData$SalePrice, xlab = 'Month Sold', 
     ylab = 'Sale', main = 'sale price by month sold')
#ggplot2 by BldgType 
bwplot( HouseData$SalePrice ~ HouseData$MSZoning, xlab = 'ZONING', 
        ylab = 'Sale', main = 'sale price by ZONING')
plot( HouseData$MSSubClass,  HouseData$SalePrice, xlab = 'MS SUBCLASS', 
      ylab = 'Sale', main = 'sale price by MS SUBCLASS')

bwplot( HouseData$SalePrice ~ HouseData$HouseStyle, xlab = 'House Style', 
        ylab = 'Sale price', main = 'sale price by House Style')
sales_each_month <- with(HouseData, table(HouseData$MoSold)) # more sales in 5,6,7 months
qplot(MoSold, data = HouseData, fill = BldgType)

barplot(sales_each_month, main = "No of sales in each month", col = 'brown')
plot(HouseData$SaleCondition, HouseData$SalePrice, xlab = 'sale condition', 
     ylab = 'Sale', main = 'sale price by sale condition')
plot(HouseData$LotFrontage, HouseData$SalePrice, xlab = 'Lot Frontage', 
     ylab = 'Sale', main = 'sale price by Lot Frontage')

print(ggplot(HouseData, aes(y = SalePrice, x = YearBuilt, colour = Foundation)) + 
        geom_smooth() + ggtitle("sale price by year built and building type"))

p1 <- ggplot(HouseData, aes(x=YearBuilt, y=SalePrice, colour=BldgType)) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("sale price by building type")
p1

sales_condition <- with(HouseData, table(HouseData$SaleCondition))
barplot(sales_condition, main = "Sales by sale condition", col = 'brown')

p2 <- ggplot(HouseData, aes(x=YearBuilt, y=SalePrice, colour=BldgType)) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("sale price by building type")
p2

xyplot(HouseData$SalePrice ~ HouseData$YearBuilt| HouseData$MoSold, 
       data = HouseData, layout = c(4,3))

#bwplot(HouseData$Saleprice_K ~ HouseData$RoofStyle , data = HouseData, 
#xlab = 'Roof Style', ylab = 'Sale', main = 'sale price by Roof Style’)
totalbsmtsf <- HouseData$BsmtUnfSF + HouseData$BsmtFinSF1 + HouseData$BsmtFinSF2
plot(totalbsmtsf, HouseData$TotalBsmtSF) 
print("TotalBsmtSF is equal to BsmtUnfSF + BsmtFinSF1 + BsmtFinSF2")

GrLivArea <- HouseData$X1stFlrSF + HouseData$X2ndFlrSF + HouseData$LowQualFinSF
plot(GrLivArea, HouseData$GrLivArea)
print("GrLivArea is equal to X1stFlrSF + X2ndFlrSF + LowQualFinSF")

table(HouseData$Fireplaces)
table(HouseData$FireplaceQu)
table(HouseData$Fireplaces, HouseData$FireplaceQu)
print("Missing values in Fireplace Quality are due to no Fire place (Fireplace = 0)")

table(HouseData$PoolQC)
table(HouseData$PoolArea)
levels(HouseData$PoolQC)
print("For pool area 0, pool quality is missing")
cor(HouseData$PoolArea, HouseData$SalePrice)
#---------------------------------------------------------------------------------
print("Removing columns with more missing values from train data and test data")
# remove ID and sale price from housedata(train data)
print("Imputing missing values Fire place quality")
HouseData$YrSold <- as.factor(HouseData$YrSold)
TestData$YrSold <- as.factor(TestData$YrSold)

print("replacing missing values in fire place quality as they are missing because of no fore place")

Level_House <- levels(HouseData$FireplaceQu)
Level_Test <- levels(TestData$FireplaceQu)
Unique_Levels <- union(Level_House, Level_Test) 
new_levels <- c(Unique_Levels, "Missing")
levels(HouseData$FireplaceQu) <- new_levels
levels(TestData$FireplaceQu) <- new_levels
HouseData[HouseData$Fireplaces == 0, "FireplaceQu"] <- "Missing"
TestData[TestData$Fireplaces == 0, "FireplaceQu"] <- "Missing"

HouseData[HouseData$MiscVal > 0, "MiscVal"] <- 1
TestData[TestData$MiscVal > 0, "MiscVal"] <- 1
HouseData$MiscVal <- as.factor(HouseData$MiscVal)
TestData$MiscVal <- as.factor(TestData$MiscVal)

data_clean <- function(Input_Data){
  Porch <- Input_Data$OpenPorchSF + Input_Data$EnclosedPorch + Input_Data$X3SsnPorch + Input_Data$ScreenPorch
  BsmtBath <- Input_Data$BsmtFullBath + (Input_Data$BsmtHalfBath/2)
  Bath <- Input_Data$FullBath + (Input_Data$HalfBath/2)
  Input_Data <- cbind(Input_Data, Porch, BsmtBath, Bath)
  return(Input_Data)
}

HouseData <- data_clean(HouseData)
TestData <- data_clean(TestData)

Drop <- c("Id", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF",  
          "LowQualFinSF","TotRmsAbvGrd", "PoolArea", "PoolQC", "MasVnrArea", "MasVnrType",
          "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "BsmtFullBath", 
          "BsmtHalfBath", "FullBath", "HalfBath")

HouseData <- HouseData[ , !(names(HouseData) %in% Drop)]
TestData <- TestData[ , !(names(TestData) %in% Drop)]
#Drop <- c("Id", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF",  "LowQualFinSF")

#---Remove columns with more missing values and cant be used in the model becasue of missing values
Remove_Columns <- function(Input_DataSet) {
  Col_Names <- colnames(Input_DataSet)
  Pmiss <- function(x){sum(is.na(x))/length(x)*100}
  Pmiss_Col <- apply(Input_DataSet,2,Pmiss)
  Rem_col <- c()
  j <- 0
  for (i in  seq_along(Col_Names)){
    if (Pmiss_Col[[i]] >= 40){
      j <- j + 1
      Rem_col[j] <- i
    }
  }
  Input_DataSet <- Input_DataSet[ -Rem_col]  
  return(Input_DataSet)
}

House_Data <- Remove_Columns(HouseData)
print("New train data summary")

HouseCol <- colnames(House_Data)
HouseCol <- HouseCol[ !(HouseCol %in% c("SalePrice"))]

HouseCol
ncol(House_Data)
colnames(TestData)
# use the same columns in testdata to send it to the regression model
Test_Data <- TestData[,c(HouseCol)]
print("New Test data summary")
head(Test_Data)

ClassData <- sapply(House_Data[c(HouseCol)], class)

# replace factors with "Missing" and integer with mean values
#--------------------------------------------------------------------------------
print("Before Imputing")
Pmiss <- function(x){sum(is.na(x))/length(x)*100}
Pmiss_Row <- apply(House_Data,1,Pmiss)
#summary(house_data)
print("Imputing missing values in train data set")

method_impute <- function(Input_data){
  f <- sapply(Input_data, class)
  method_vector <- c()
  for(i in seq_along(f))
    if(f[[i]] == "factor" && length(levels(Input_data[[i]])) == 2)
      method_vector[i] <- c("logreg")
  else 
    method_vector[i] <- c("cart")
  return(method_vector)
}


Imp_HouseData <- mice(House_Data, m=1, method=method_impute(House_Data), printFlag=FALSE)
Imp_TestData <- mice(Test_Data, m=1, method=method_impute(Test_Data), printFlag=FALSE)

Imp_TestData$Imp$Utilities
HouseDataImp <- complete(Imp_HouseData)
TestDataImp <- complete(Imp_TestData)

#------------------------------------------------------------------------------
# Dealing with skewness and kurtosis
skew_data <- sapply(HouseDataImp, skewness, na.rm = TRUE)
skew_data
Kurt_data <- sapply(HouseDataImp, kurtosis, na.rm = TRUE)
Kurt_data

print("Lot area is highly right skewed")

data_log <- function(Input_Data){
  LotAreaLog <- log(Input_Data$LotArea)
  #MiscValLog <- log(Input_Data$MiscVal + 1)
  LotFrontageLog <- log(Input_Data$LotFrontage)
  KitchenAbvGrLog <- log(Input_Data$KitchenAbvGr + 1)
  Input_Data <- cbind(Input_Data, LotAreaLog)
  #Input_Data <- cbind(Input_Data, MiscValLog)
  Input_Data <- cbind(Input_Data, LotFrontageLog)
  Input_Data <- cbind(Input_Data, KitchenAbvGrLog)
  Input_Data <- Input_Data[ , !(names(Input_Data) %in% c("LotArea", "LotFrontage", "KitchenAbvGr"))]
  return(Input_Data)
}
HouseDataImp <- data_log(HouseDataImp)
TestDataImp <- data_log(TestDataImp)

skew_data <- sapply(HouseDataImp, skewness, na.rm = TRUE)
skew_data
Kurt_data <- sapply(HouseDataImp, kurtosis, na.rm = TRUE)
Kurt_data

#Lot Area
hist(HouseDataImp$LotAreaLog, breaks = 40, xlab = 'LotArea',
     main = paste("Histogram of" , "Lot Area"), col = 'pink4')


#----------------------------------------------------------------------------
Pmiss <- function(x){sum(is.na(x))}
Pmiss_col2 <- apply(TestDataImp,2,Pmiss)
for(i in seq_along(Pmiss_col2)){
  if(Pmiss_col2[[i]] > 0 && ClassData[[i]] == "factor" && length(levels(TestDataImp[, i])) == 1)
    TestDataImp[is.na(TestDataImp[, i]), i] <- levels(TestDataImp[, i])
}

sjp.corr(HouseDataImp[, sapply(HouseDataImp, is.numeric)])

col_select <- c()
col_name <- colnames(HouseDataImp)[!colnames(HouseDataImp) %in% "SalePrice"]
l <- 1
for (i in seq_along(col_name)){
  if(is.factor(HouseDataImp[,col_name[i]])){
    p <- lm(HouseDataImp$SalePrice ~ HouseDataImp[,col_name[i]])
    if (summary(p)$r.squared >= 0.3 | summary(p)$r.squared <= -0.3){
      col_select[l] <- col_name[i]
      l <- l+1
    }
  }
  else
    if (cor(HouseDataImp$SalePrice, HouseDataImp[,col_name[i]]) >= 0.3){
      col_select[l] <- col_name[i]
      l <- l+1
    }
}


high_cor_col <- c("LotFrontageLog", "GarageArea", "GarageYrBlt", "Bath", "YearRemodAdd")
col_select <- setdiff(col_select, high_cor_col)
#removing highly correlated variables

TestData_Final <- TestDataImp[c(col_select)]
#col_select <- union(col_select, "SalePrice") 
HouseData_Final <- HouseDataImp[c(col_select)]

ClassData <- sapply(HouseData_Final, class)

for(i in seq_along(ClassData)) {
  if (ClassData[[i]] == "factor") {
    Level_House <- levels(HouseData_Final[,i])
    Level_Test <- levels(TestData_Final[,i])
    Unique_Levels <- union(Level_House, Level_Test) 
    levels(HouseData_Final[,i]) <- Unique_Levels
    levels(TestData_Final[,i]) <- Unique_Levels
  }
}

SalePrice <- HouseDataImp$SalePrice
HouseData_Final <- cbind(HouseData_Final, SalePrice)
sjp.corr(HouseData_Final[, sapply(HouseData_Final, is.numeric)])


print("predicting sale price using Random Forest")
rf <- randomForest(SalePrice ~.,HouseData_Final, do.trace=TRUE)
p <- predict(rf,TestData_Final)
submission$SalePrice <- p
# write.csv(submission,file="submission.csv",row.names=FALSE)