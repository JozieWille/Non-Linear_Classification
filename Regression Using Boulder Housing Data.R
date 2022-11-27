# Linear regression on boulder housing data
----------------------------------------------------------------------------------
load("/Users/joziewille/Downloads/5415 Advanced Data Analytics/boulder-cleaned.RData")

load("/Users/joziewille/Downloads/5415 Advanced Data Analytics/boulder-cleaned.RData")
str(boulder.clean) 
----------------------------------------------------------------------------------
# examples of apply and tapply
apply(boulder.clean[, c("BEDS", "BATHS")], 2, sd, na.rm = TRUE)
lapply(boulder.clean[, c("BEDS", "BATHS")], sd, na.rm = TRUE)
sapply(boulder.clean[, c("BEDS", "BATHS")], sd, na.rm = TRUE)
tapply(boulder.clean$LIST.PRICE, boulder.clean$BEDS, mean)

# Average Square feet for houses with different parking types
tapply(boulder.clean$SQFT, boulder.clean$PARKING.TYPE, mean)
----------------------------------------------------------------------------------
# removing rows with na values
colSums(is.na(boulder.clean))
boulder.clean <- na.omit(boulder.clean)

# focusing on single family residential homes and houses with list price
# less than 3 million dollars
boulder.sfr <- subset(boulder.clean, HOME.TYPE == "Single Family Residential", 
                      select = c(LIST.PRICE, SQFT, PARKING.TYPE))
boulder.sfr <- subset(boulder.sfr, LIST.PRICE < 3000)

# boxplot of LIST.PRICE
boxplot(boulder.sfr$LIST.PRICE)

# scatter plot: SQFT vs. LIST.PRICE
with(boulder.sfr, plot(SQFT, LIST.PRICE))

# side by side boxplot of LIST.PRICE for different PARKING.TYPE
with(boulder.sfr, boxplot(LIST.PRICE ~ PARKING.TYPE))

# Linear regression model of LIST.PRICE on SQFT and PARKING.TYPE
lm.fit <- lm(LIST.PRICE ~ SQFT + PARKING.TYPE, data = boulder.sfr)
summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)
par(mfrow = c(1, 1))
----------------------------------------------------------------------------------
# adding interaction terms between SQFT and PARKING.TYPE (*multiplication)
lm.fit1 <- lm(LIST.PRICE ~ SQFT * PARKING.TYPE, data = boulder.sfr)
summary(lm.fit1)
----------------------------------------------------------------------------------
# data transformation
lm.fit1 <- lm(LIST.PRICE ~ SQFT + PARKING.TYPE + I(SQFT^2), data = boulder.sfr)
summary(lm.fit1)

# data transformation with log of square feet
lm.fit1 <- lm(LIST.PRICE ~ log(SQFT) + PARKING.TYPE, data = boulder.sfr)
summary(lm.fit1)
----------------------------------------------------------------------------------
# transforming the target variable
lm.fit1 <- lm(log(LIST.PRICE) ~ SQFT + PARKING.TYPE, data = boulder.sfr)
summary(lm.fit1)

# transformation
lm.fit1 <- lm(log(LIST.PRICE) ~ log(SQFT) + PARKING.TYPE, data = boulder.sfr)
summary(lm.fit1)
----------------------------------------------------------------------------------
# validation 
set.seed(300)

# sample: which gives vector of numbers
# 0.6 = sampling 60% of the data
train <- sample(nrow(boulder.sfr), nrow(boulder.sfr) * 0.6)
----------------------------------------------------------------------------------
# applying subset parameter to only use training data when building the model
lm.fit <- lm(LIST.PRICE ~ SQFT + PARKING.TYPE, data = boulder.sfr, 
               subset = train)

# Alternative way shown below:

# specified number of rows are only in training
# residual vale = observed - predicted
lm.fit <- lm(LIST.PRICE ~ SQFT + PARKING.TYPE, data = boulder.sfr[train, ])
----------------------------------------------------------------------------------
# mean of squared error 
mean((boulder.sfr$LIST.PRICE - predict(lm.fit, boulder.sfr))[-train]^2)
----------------------------------------------------------------------------------
# Multi-Fold Cross Validation / 10-fold cross validation
# bias variance trade off
  library(boot)
set.seed(300)

lm.fit <- glm(LIST.PRICE ~ SQFT + PARKING.TYPE, data = boulder.sfr)

cv.error <- cv.glm(boulder.sfr, lm.fit, K = 10)$delta[1]
----------------------------------------------------------------------------------
# multiple regression
boulder.sfr <- subset(boulder.clean, HOME.TYPE == "Single Family Residential", 
                        select = -c(HOME.TYPE, ADDRESS))

boulder.sfr <- na.omit(boulder.sfr)
boulder.sfr <- subset(boulder.sfr, LIST.PRICE < 3000)
----------------------------------------------------------------------------------
# correlation matrix
cor(subset(boulder.sfr, select = -c(ZIP, PARKING.TYPE)))
----------------------------------------------------------------------------------
# pairwise scatter plots
pairs(subset(boulder.sfr, select = -c(ZIP, PARKING.TYPE)))
----------------------------------------------------------------------------------
# multiple regression with all variables
lm.fit <- lm(LIST.PRICE ~ ., data = boulder.sfr)
----------------------------------------------------------------------------------
# MODEL SELECTION

# best subset selection
library(leaps)
lm.fit.full <- regsubsets(LIST.PRICE ~ ., data = boulder.sfr)
reg.summary <- summary(lm.fit.full)
----------------------------------------------------------------------------------
# adjusted rsquare
plot(reg.summary$adjr2)
----------------------------------------------------------------------------------
  # Mallows's Cp -- equivalent to AIC (Akaike information criterion)
  # under some conditions; smaller is better
  plot(reg.summary$cp)
----------------------------------------------------------------------------------
  # BIC (Bayesian information criterion); smaller is better
  plot(lm.fit.full, scale = "bic")
----------------------------------------------------------------------------------
  # forward selection
  lm.fit.forward <- regsubsets(LIST.PRICE ~ ., data = boulder.sfr, 
                               method = "forward")
----------------------------------------------------------------------------------
  # backward selection
  lm.fit.backward <- regsubsets(LIST.PRICE ~ ., data = boulder.sfr, 
                                method = "backward")