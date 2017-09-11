
# The data directory contains 4 files:
#   - data_description.txt
#   - test.csv
#   - train.csv
#   - sample_submission.csv

##### Clean Up Workspace #####
rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection

########## Install and/or Load Packages ##########
packages <- function(x, repos = "http://cran.r-project.org", ...) {
  x <- deparse(substitute(x))
  if (!require(x, character.only = TRUE)) {
    install.packages(pkgs = x, dependencies = TRUE, repos = repos, ...)
    library(x, character.only = TRUE)
  }
}

# Load libraries
# update.packages(repos = "http://cran.r-project.org") # Updates Packages (Do Periodically)
packages(data.table) # Data Frame Complement
packages(doParallel) # Parallel Computing
packages(foreach) # Parallel Computing
packages(jsonlite) # JSON Data
packages(reshape2) # Manipulate Datasets
packages(pdftools) # PDF to TXT Editor
packages(splitstackshape) # Stack and Reshape Datasets After Splitting Concatenated Values
packages(stringi) # Character/String Editor
packages(stringr) # Character/String Editor
packages(tm) # Text Mining
packages(dplyr) # Splitting, applying, and combining data
packages(boot) # Contains cv.glm
packages(leaps) # For regsubsets
packages(ggplot2)
packages(glmnet)
packages(forcats)
packages(caret)

##### Define functions #####

# Changes NAs to None for Categorical and 0 for Numeric
munge <- function(df) {
  require(forcats)
  
  tdf <- df
  
  colClasses <- sapply(tdf, class)
  
  #CATEGORICAL ADD NA TO LEVELS
  tdf[colClasses =="factor"] <- lapply(tdf[colClasses == "factor"], function(x) fct_explicit_na(x, "None") )
  
  #NUMERIC TURN NAS TO 0
  tdf[colClasses != "factor"] <- lapply(tdf[colClasses != "factor"], function(x) { x[is.na(x)] <- 0; x})  
  
  tdf
}

# Cost function: Root Mean Squared Logarithmic Error
rmsle <- function(yhat, y) {
  return(sqrt(mean((yhat - y) ** 2)))
}

# Function to return variable names related to regsubsets output
var_select <- function (x, end) {
  vars <- c()
  begin <- names(x)
  for (begin.feature in begin) {
    for (end.feature in end) {
      if (startsWith(end.feature, begin.feature)) {
        vars <- union(vars, begin.feature)
      } 
    }
  }
  return(vars)
}

# Sets ordinality in categorical variables (improves prediction accuracy)
fixOrdinals <- function(df) {
  df$LotShape     <- factor(df$LotShape,     c("IR3", "IR2", "IR1", "Reg"), ordered = TRUE)
  df$Utilities    <- factor(df$Utilities,    c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = TRUE)
  df$LandSlope    <- factor(df$LandSlope,    c("Sev", "Mod", "Gtl"), ordered = TRUE)
  df$ExterQual    <- factor(df$ExterQual,    c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$ExterCond    <- factor(df$ExterCond,    c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$BsmtQual     <- factor(df$BsmtQual,     c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$BsmtCond     <- factor(df$BsmtCond,     c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$BsmtExposure <- factor(df$BsmtExposure, c("None", "No", "Mn", "Av", "Gd"), ordered = TRUE)
  df$BsmtFinType1 <- factor(df$BsmtFinType1, c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE)
  df$BsmtFinType2 <- factor(df$BsmtFinType2, c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE)
  df$HeatingQC    <- factor(df$HeatingQC,    c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$Electrical   <- factor(df$Electrical,   c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"), ordered = TRUE)
  df$KitchenQual  <- factor(df$KitchenQual,  c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$Functional   <- factor(df$Functional,   c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal"), ordered = TRUE)
  df$FireplaceQu  <- factor(df$FireplaceQu,  c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$GarageFinish <- factor(df$GarageFinish, c("None", "Unf", "RFn", "Fin"),  ordered = TRUE)
  df$GarageQual   <- factor(df$GarageQual,   c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$GarageCond   <- factor(df$GarageCond,   c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$PavedDrive   <- factor(df$PavedDrive,   c("N", "P", "Y"), ordered = TRUE)
  df$PoolQC       <- factor(df$PoolQC, c("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  df$Electrical[df$Electrical   == "Mix"]  <- "FuseP"
  df$Utilities[df$Utilities     == "None"] <- "AllPub"
  df$BsmtQual[df$BsmtQual       == "None"] <- "Fa"
  df$BsmtCond[df$BsmtCond       == "None"] <- "Fa"
  df$KitchenQual[df$KitchenQual == "None"] <- "Fa"
  df$Functional[df$Functional   == "None"] <- "Typ"
  df$FireplaceQu[df$FireplaceQu == "None"] <- "Fa"
  df$GarageQual[df$GarageQual   == "None"] <- "Fa"
  df$GarageCond[df$GarageCond   == "None"] <- "Fa"
  df
}


##### Set up the data set #####

# Set working directory
setwd("/Users/jholc89/Google Drive/R/house_prices/files")

# Read training and test set
train <- read.csv("train.csv")
test <- read.csv("test.csv")


##### Feature engineering #####

# Impute 0 for NAs
train <- munge(train)
test  <- munge(test)

# Fix Ordinals
train <- fixOrdinals(train)
test  <- fixOrdinals(test)

# Quadratic terms
train$YearBuilt2  <- train$YearBuilt**2
test$YearBuilt2   <- test$YearBuilt**2

# Impute specific values in the training set
train[which(train$GarageYrBlt == 0), "GarageYrBlt"] <- train$YearBuilt

# Impute specific values in the test set
test[which(test$MSSubClass == 150), "MSSubClass"] <- 160
test[which(test$GarageYrBlt == 0), "GarageYrBlt"] <- test$YearBuilt

# Impute missing Electrical
train[which(is.na(train$Electrical)), "Electrical"] <- "SBrkr"

# Reduce MasVnrType
train$MasVnrType[train$MasVnrType == "BrkCmn"] <- "None"
test$MasVnrType[test$MasVnrType   == "BrkCmn"] <- "None"

# Reduce Land Contour
train$LandContour[train$LandContour == "Bnk"] <- "Lvl"
train$LandContour[train$LandContour == "Low"] <- "HLS"
test$LandContour[test$LandContour   == "Bnk"] <- "Lvl"
test$LandContour[test$LandContour   == "Low"] <- "HLS"

# Reduce Lot Config
train$LotConfig[train$LotConfig == "FR3"] <- "FR2"
test$LotConfig[test$LotConfig   == "FR3"] <- "FR2"

# Reduce LotShape to Reg or IR
train$LotShape <- as.factor(with(train, ifelse(LotShape == "Reg", "Reg", "IR")))
test$LotShape  <- as.factor(with(test,  ifelse(LotShape == "Reg", "Reg", "IR")))

# Reduce LandSlope to Gtl or NotGtl
train$LandSlope <- as.factor(with(train, ifelse(LandSlope == "Gtl", "Gtl", "NotGtl")))
test$LandSlope  <- as.factor(with(test,  ifelse(LandSlope == "Gtl", "Gtl", "NotGtl")))

# Reduce MiscFeature to Shed or No Shed
train$MiscFeature[train$MiscFeature != "None"] <- "Shed"
test$MiscFeature[test$MiscFeature   != "None"] <- "Shed"

# Reduce Condition 1
train$Condition1[train$Condition1 == "PosA"] <- "PosN"
train$Condition1[train$Condition1 %in% c("RRAe", "RRAn", "RRNe", "RRNn")] <- "RRAn"
test$Condition1[test$Condition1   == "PosA"] <- "PosN"
test$Condition1[test$Condition1   %in% c("RRAe", "RRAn", "RRNe", "RRNn")] <- "RRAn"

# Reduce Foundation
train$Foundation[train$Foundation %in% c("Slab", "Stone", "Wood")] <- "Slab"
test$Foundation[test$Foundation   %in% c("Slab", "Stone", "Wood")] <- "Slab"

# Heating
train$Heating[train$Heating == "GasW"] <- "GasA"
train$Heating[train$Heating != "GasA"] <- "OthW"

# Change MSSubClass to categorical
train$MSSubClass <- as.factor(train$MSSubClass)
test$MSSubClass  <- as.factor(test$MSSubClass)

# Remove observations with outliers
train <- train[which(train$GrLivArea   < 4000), ]    # Total square footage
train <- train[-which(train$LotFrontage > 300), ]     # Lot frontage
train <- train[which(train$LotArea     < 200000), ]  # Lot area

# Log transform variables
train$SalePrice     <- log(train$SalePrice + 1)
train$LotArea       <- log(train$LotArea + 1)
train$GrLivArea     <- log(train$GrLivArea + 1)
train$LotFrontage   <- log(train$LotFrontage + 1)
train$TotalBsmtSF   <- log(train$TotalBsmtSF + 1)
test$LotArea        <- log(test$LotArea + 1)
test$GrLivArea      <- log(test$GrLivArea + 1)
test$LotFrontage    <- log(test$LotFrontage + 1)
test$TotalBsmtSF    <- log(test$TotalBsmtSF + 1)


##### Matrices #####

# Separate feature matrix and response vector from training data
X <- select(train, -SalePrice, -Id)
X <- model.matrix(~ ., data = X)
Y <- train[, 81]

# Matrix for test data
X.test <- select(test, -Id)
X.test <- model.matrix(~ ., data = X.test)

# Remove columns from train not in test
X <- X[, -which(colnames(X) %in% setdiff(colnames(X), colnames(X.test)))]
X.test <- X.test[, -which(colnames(X.test) %in% setdiff(colnames(X.test), colnames(X)))]


##### Models #####

# Lasso fit
set.seed(123)
lasso.fit <- cv.glmnet(x=X, y=Y, alpha=1)
plot(lasso.fit)

# Ridge regression fit
# Compute CV error for a grid of lambda solutions and plot
ridge.fit <- cv.glmnet(x=X, y=Y, alpha=0)
lambda.hat <- ridge.fit$lambda.min
plot(x = log(ridge.fit$lambda), y = ridge.fit$cvm, xlab = 'log(lambda)', ylab = 'CV error',
     main = 'CV Errors for Lambda using Ridge (Default Grid)', type = 'l')
abline(v = log(lambda.hat))

# Fix grid
lambda.new <- seq(lambda.hat, lambda.hat * .01, length = 100)
ridge.fit <- cv.glmnet(x=X, y=Y, alpha=0, lambda=lambda.new)
lambda.hat <- ridge.fit$lambda.min
plot(x = ridge.fit$lambda, y = ridge.fit$cvm, xlab = 'lambda', ylab = 'CV error',
     main = 'CV Errors for Lambda using Ridge (Corrected Grid)', type = 'l')
abline(v = ridge.fit$lambda.min)

# Refitted lasso
select.feats <- which(abs(coef(lasso.fit, s='lambda.1se'))[-1] > 1e-16)
refit.fit <- lm(Y ~ ., data = data.frame(X[, select.feats]))


###################################
##### Produce submission file #####
###################################

# Check training error
yhat.train.ridge <- predict(ridge.fit, newx = X, s = 'lambda.min') 
yhat.train.lasso <- predict(lasso.fit, newx = X, s = 'lambda.min') 
yhat.train.refit <- predict(refit.fit, newdata = data.frame(X))
rmsle(yhat.train.ridge, train$SalePrice)
rmsle(yhat.train.lasso, train$SalePrice)
rmsle(yhat.train.refit, train$SalePrice)

# Predict house prices (yhat)
yhat <- exp(predict(ridge.fit, newx = X.test, s = 'lambda.min')) # Kaggle score: .11771
yhat <- exp(predict(lasso.fit, newx = X.test, s = 'lambda.min')) # Kaggle score: .12070
yhat <- exp(predict(refit.fit, newdata = data.frame(X.test)))    # Kaggle score: .12231

# Average ridge and lasso predictions
yhat.ridge <- exp(predict(ridge.fit, newx = X.test, s = 'lambda.min')) 
yhat.lasso <- exp(predict(lasso.fit, newx = X.test, s = 'lambda.min')) 
yhat.refit <- exp(predict(refit.fit, newdata = data.frame(X.test)))    
yhat       <- (yhat.ridge + yhat.lasso + yhat.refit) / 3

# Create submission.csv file
yhat
any(is.na(yhat))
prediction <- data.frame(test$Id, yhat)
colnames(prediction) <- c("Id", "SalePrice")
write.csv(prediction, "submission.csv", row.names = FALSE, quote = FALSE)

# Write matrix to .csv
write.csv(X, "train_mat.csv", row.names = F, quote = F)
write.csv(X.test, "test_mat.csv", row.names = F, quote = F)



##### Explore the training data set #####

# Missing values, duplicate data, etc.
str(train) # Structure of the df: # of obs, # of variables, types of variables
any(is.na(train)) # TRUE if missing values exist, FALSE otherwise
colSums(sapply(train, is.na)) # Number of missing values per column
sum(is.na(train)) / (nrow(train) * ncol(train)) # Percentage of values that are missing
nrow(train) - nrow(unique(train)) # Number of duplicate rows

# Area graphs of numeric variables ***CONSIDER sqrt transform of sale price to correct heteroskedasticity***
ggplot(train, aes(SalePrice))   + geom_area(stat = "bin") # Sale price
ggplot(train, aes(GrLivArea))   + geom_area(stat = "bin") # Square footage
ggplot(train, aes(LotArea))     + geom_area(stat = "bin") # Lot area
ggplot(train, aes(TotalBsmtSF)) + geom_area(stat = "bin") # Basement square footage
ggplot(train, aes(LotFrontage)) + geom_area(stat = "bin") # Lot frontage

# Scatterplots with numerical variables against sale price
ggplot(train %>% filter(LotFrontage > 0), aes(x = LotFrontage,     y = SalePrice)) + geom_point() + geom_smooth(method = lm) # Lot frontage
ggplot(train, aes(x = LotArea,         y = SalePrice)) + geom_point() # Lot area
ggplot(train, aes(x = OverallQual,     y = SalePrice)) + geom_point() # Overall quality
ggplot(train, aes(x = OverallCond,     y = SalePrice)) + geom_point() # Overall condition
ggplot(train, aes(x = YearBuilt,       y = SalePrice)) + geom_point() # Year built
ggplot(train, aes(x = YearRemodAdd,    y = SalePrice)) + geom_point() # Year remodeled
ggplot(train, aes(x = MasVnrArea,      y = SalePrice)) + geom_point() # Masonry veneer area
ggplot(train, aes(x = ExterQual,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = ExterCond,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtQual,        y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtCond,        y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtExposure,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtFinType1,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtFinSF1,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtFinType2,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtFinSF2,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtUnfSF,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = TotalBsmtSF,     y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = HeatingQC,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = X1stFlrSF,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = X2ndFlrSF,       y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = LowQualFinSF,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = GrLivArea,       y = SalePrice)) + geom_point() # Square footage
ggplot(train, aes(x = BsmtFullBath,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = BsmtHalfBath,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = FullBath,        y = SalePrice)) + geom_point() # Number of full baths
ggplot(train, aes(x = HalfBath,        y = SalePrice)) + geom_point() # Number of half baths
ggplot(train, aes(x = BedroomAbvGr,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = KitchenAbvGr,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = KitchenQual,     y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = TotRmsAbvGrd,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = Functional,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = Fireplaces,      y = SalePrice)) + geom_point() # Number of fireplaces
ggplot(train, aes(x = FireplaceQu,     y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = GarageYrBlt,     y = SalePrice)) + geom_point() # Year garage was built
ggplot(train, aes(x = GarageFinish,    y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = GarageCars,      y = SalePrice)) + geom_point() # Car capacity of garage
ggplot(train, aes(x = GarageArea,      y = SalePrice)) + geom_point() # Area of garage
ggplot(train, aes(x = GarageQual,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = GarageCond,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = WoodDeckSF,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = OpenPorchSF,     y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = EnclosedPorch,   y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = X3SsnPorch,      y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = ScreenPorch,     y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = PoolArea,        y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = PoolQC,          y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = Fence,           y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = MiscVal,         y = SalePrice)) + geom_point() # 
ggplot(train, aes(x = MoSold,          y = SalePrice)) + geom_point() # Month sold
ggplot(train, aes(x = YrSold,          y = SalePrice)) + geom_point() # Year sold

# Box plots with categorical variables against sale price
ggplot(train, aes(x = MSSubClass,    y = SalePrice)) + geom_boxplot() # Type of dwelling
ggplot(train, aes(x = MSZoning,      y = SalePrice)) + geom_boxplot() # Zoning classification
ggplot(train, aes(x = Street,        y = SalePrice)) + geom_boxplot() # Type of road access
ggplot(train, aes(x = Alley,         y = SalePrice)) + geom_boxplot() # Type of alley access
ggplot(train, aes(x = LotShape,      y = SalePrice)) + geom_boxplot() # Shape of property
ggplot(train, aes(x = LandContour,   y = SalePrice)) + geom_boxplot() # Flatness of property
ggplot(train, aes(x = Utilities,     y = SalePrice)) + geom_boxplot() # Utilities available
ggplot(train, aes(x = LotConfig,     y = SalePrice)) + geom_boxplot() # Lot configuration
ggplot(train, aes(x = LandSlope,     y = SalePrice)) + geom_boxplot() # Slope of property
ggplot(train, aes(x = Neighborhood,  y = SalePrice)) + geom_boxplot() # Neighborhood
ggplot(train, aes(x = Condition1,    y = SalePrice)) + geom_boxplot() # Proximity to various conditions
ggplot(train, aes(x = Condition2,    y = SalePrice)) + geom_boxplot() # Proximity to various conditions (2)
ggplot(train, aes(x = BldgType,      y = SalePrice)) + geom_boxplot() # Type of dwelling
ggplot(train, aes(x = HouseStyle,    y = SalePrice)) + geom_boxplot() # Style of dwelling
ggplot(train, aes(x = RoofStyle,     y = SalePrice)) + geom_boxplot() # Type of roof
ggplot(train, aes(x = RoofMatl,      y = SalePrice)) + geom_boxplot() # Roof material
ggplot(train, aes(x = Exterior1st,   y = SalePrice)) + geom_boxplot() # Exterior covering
ggplot(train, aes(x = Exterior2nd,   y = SalePrice)) + geom_boxplot() # Exterior covering (2)
ggplot(train, aes(x = MasVnrType,    y = SalePrice)) + geom_boxplot() # Masonry veneer type
ggplot(train, aes(x = Foundation,    y = SalePrice)) + geom_boxplot() # Type of foundation
ggplot(train, aes(x = Heating,       y = SalePrice)) + geom_boxplot() # Type of heating
ggplot(train, aes(x = CentralAir,    y = SalePrice)) + geom_boxplot() # Central air conditioning
ggplot(train, aes(x = Electrical,    y = SalePrice)) + geom_boxplot() # Electrical system
ggplot(train, aes(x = GarageType,    y = SalePrice)) + geom_boxplot() # Garage type
ggplot(train, aes(x = PavedDrive,    y = SalePrice)) + geom_boxplot() # Paved driveway
ggplot(train, aes(x = MiscFeature,   y = SalePrice)) + geom_boxplot() # Miscellaneous feature
ggplot(train, aes(x = SaleType,      y = SalePrice)) + geom_boxplot() # Type of sale
ggplot(train, aes(x = SaleCondition, y = SalePrice)) + geom_boxplot() # Condition of sale


##### Extra stuff #####

# Bin the sale price into groups
price.bin <- cut(Y$SalePrice, breaks = 50)

# Distinguish between categorical and numerical variables
train.cat <- select(train, which(sapply(train, is.factor)))
train.num <- select(train, which(sapply(train, is.numeric)))
