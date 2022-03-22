data <- read.table("Heart attack possibility.csv", head=T, fileEncoding = 'UTF-8-BOM', sep=",")

head(data)
dim(data)


##EDA 


# Correlation matrix between factors

cormat <- round(cor(data[,1:13]), 2)

library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormat, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

cormatnum <- round(cor(data[,c(1,4,5,8,10,12)]), 2)

library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormatnum, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

cormatcat <- round(cor(data[,c(2,3,6,7,9,11,13)]), 2)

library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormatcat, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

## Dividing the data set into training and testing data set

set.seed(12345678);
tflag <- sort(sample(1:303,75));
datatest <- data[tflag, ];
datatrain <- data[-tflag, ];




## Model / Variable Selection

# Elementary Model Selection Methods

# Marginal association of each factor with target

# 1) 
summary(glm(TARGET ~ AGE, family=binomial(link=logit), data=datatrain))
# Pvalue = 0.000109

# 2) 
summary(glm(TARGET~ SEX, family=binomial(link=logit), data=datatrain))
# Pvalue = 0.000177

# 3) 
summary(glm(TARGET~ CP, family=binomial(link=logit), data=datatrain))
# Pvalue =  3.90e-10

# 4) 
summary(glm(TARGET~ TRESTBPS, family=binomial(link=logit), data=datatrain))
# Pvalue =  0.0150

# 5) 
summary(glm(TARGET~ CHOL, family=binomial(link=logit), data=datatrain))
# Pvalue =   0.215

# 6) 
summary(glm(TARGET~ FBS, family=binomial(link=logit), data=datatrain))
# Pvalue =   0.336

# 7) 
summary(glm(TARGET~ RESTECG, family=binomial(link=logit), data=datatrain))
# Pvalue =   0.555

# 8) 
summary(glm(TARGET~ THALACH, family=binomial(link=logit), data=datatrain))
# Pvalue =   1.91e-09

# 9) 
summary(glm(TARGET~ EXANG, family=binomial(link=logit), data=datatrain))
# Pvalue =   3.23e-09

# 10) 
summary(glm(TARGET~ OLDPEAK, family=binomial(link=logit), data=datatrain))
# Pvalue =   7.50e-09

# 11) 
summary(glm(TARGET~ SLOPE, family=binomial(link=logit), data=datatrain))
# Pvalue =   3.31e-08

# 12) 
summary(glm(TARGET~ CA, family=binomial(link=logit), data=datatrain))
# Pvalue =   2.74e-09

# 13) 
summary(glm(TARGET~ THAL, family=binomial(link=logit), data=datatrain))
# Pvalue =   2.54e-07


# Drop1 test
drop1(logitmodel, test="F");

# The drop test suggests that we can drop AGE, TRESTBPS, CHOL, FBS, RESTECG, THALACH, SLOPE


# Model Selection Methods using an optimization criteria and a search strategy

# 1) Forward Backward selection. AIC using step()

logitmodel.step <- step(logitmodel)
summary(logitmodel.step)

# 2) Mellows Cp

options(max.print=999999)
install.packages("leaps")
library(leaps)
leaps(data[, 1:13], data[,14])

# Cp minimum at combination row 89 
# ï..AGE  SEX    CP  TRESTBPS CHOL FBS   RESTECG THALACH EXANG OLDPEAK SLOPE   CA   THAL
# FALSE  TRUE  TRUE  TRUE   FALSE FALSE FALSE    TRUE   TRUE   TRUE    TRUE  TRUE  TRUE
# Same model as suggested by AIC

# 3) Best Subset Algorithm using Leaps(). Criteria - Mallows Cp

options(max.print=999999)
library(leaps)
leaps(data[, 1:13], data[,14])

# Cp minimum at combination row 89 
#  AGE   SEX    CP    TRESTBPS   CHOL    FBS   RESTECG    THALACH   EXANG   OLDPEAK   SLOPE   CA   THAL
# FALSE  TRUE  TRUE     TRUE    FALSE   FALSE   FALSE      TRUE      TRUE    TRUE     TRUE   TRUE  TRUE


# Stepwise search strategy using step(). Criteria - AIC

logitmodel.step <- step(logitmodel)
summary(logitmodel.step)

# The AIC() test suggests that we can drop AGE, CHOL, FBS, RESTECG

## lasso
x <- model.matrix(TARGET~., datatrain)[,-1]
y <- datatrain$TARGET
install.packages("glmnet")
library(glmnet)
install.packages("plotmo")
library(plotmo)
fit.lasso <- glmnet(x, y, family = "binomial", nfolds = 5, alpha = 1, standardize = TRUE)
plot_glmnet(fit.lasso, label = 13)

## Ridge regression

fit.ridge <- glmnet(x, y, family = "binomial", nfolds = 5, alpha = 0, standardize = TRUE)
plot_glmnet(fit.ridge, xvar = "rlambda", label = 13)

#Cross-validation for ridge
lamr <- cv.glmnet(x, y, family = "binomial", nfolds = 5, alpha = 0, standardize = TRUE)
plot(lamr)
lambdar <- l.min <- lamr$lambda.min
lambdar  #log(lambda) = -3.81 gives lambda = 0.02253

#Cross-validation for lasso
laml <- cv.glmnet(x, y, family = "binomial", nfolds = 5, alpha = 1, standardize = TRUE)
plot(laml)
lambdal <- l.min <- laml$lambda.min
lambdal  #log(lambda) = -4.2  gives lambda = 0.01498

## model with parameters selected from lasso and ridge regression
summary(glm(TARGET ~ SEX + CP + EXANG + OLDPEAK + SLOPE + CA + THAL, family=binomial(link=logit), data=datatrain))




### Model fitting

## 1) Logistic Regression with all variables- Logit model

logitmodel1 <- glm(TARGET~ ., family=binomial(link=logit), data=datatrain)
summary(logitmodel1)

# Coefficients:
#            Estimate  Std. Error  z value  Pr(>|z|)    
#(Intercept)  5.389782   3.139520   1.717 0.086024 .  
#AGE         -0.024378   0.026101  -0.934 0.350320    
#SEX         -1.648070   0.557146  -2.958 0.003096 ** 
#CP           0.875275   0.216950   4.034 5.47e-05 ***
#TRESTBPS    -0.018298   0.011933  -1.533 0.125191    
#CHOL        -0.003846   0.004366  -0.881 0.378440    
#FBS         -0.144372   0.644597  -0.224 0.822779    
#RESTECG     -0.095957   0.405988  -0.236 0.813158    
#THALACH      0.019398   0.012311   1.576 0.115087    
#EXANG       -0.792124   0.465757  -1.701 0.088995 .  
#OLDPEAK     -0.533525   0.241606  -2.208 0.027227 *  
#SLOPE        0.650475   0.414988   1.567 0.117009    
#CA          -0.949177   0.244761  -3.878 0.000105 ***
#THAL        -1.131272   0.342005  -3.308 0.000940 ***



## 2) Logistic Regression with model selected using the search strategy and selection criteria

logitmodel2 <- step(logitmodel1)
summary(logitmodel2)



## Deciding a cutoff value for the response variable in logitmodel1

Ytrue <- t(datatest[14])

testpred1 <- predict(logitmodel1, datatest, type="response"); 
testpred1


# (i) Using 0.5 as cutoff value
Ypred1 <- (testpred1 >= 0.5)
table(Ytrue, Ypred1)
mean(Ytrue != Ypred1)
# Error Rate = 0.1733


#(ii) we may choose the cutoff as  the proportion of Y=1 from the training data
Ytrain <- datatrain[14]
cutoffemp <- sum(Ytrain == 1)/dim(Ytrain)[1]
cutoffemp  # 0.5175

Ypred2 <- (testpred1 >= cutoffemp)
table(Ytrue,  Ypred2) 
mean( Ytrue != Ypred2)
# Error Rate = 0.1733


#(iii) Find the best cutoff from the training data
Ytrain <- datatrain[14]
trainpred1 <- predict(logitmodel1, datatrain, type="response") 

cutoff1 <- seq(0,1,0.001)
NN1 <- length(cutoff1);
trainerr1 <- NULL;
for (i in 1:NN1) trainerr1 <- c(trainerr1, mean( Ytrain != (trainpred1 >= cutoff1[i])) );
plot(cutoff1, trainerr1,"l", xlab = "Cutoff", ylab="Mean Prediction Error");
flagopt <- which.min(trainerr1); 
cutoffopt <- cutoff1[flagopt];
cutoffopt;  # 0.557

Ypred3 <- (testpred1 >= cutoffopt)
table(Ytrue,  Ypred3) 
mean( Ytrue != Ypred3)
# Error Rate = 0.1733


# (iv) USing validation data to choose cutoff value. 
# Suppose now if we treat the test data as a validation data we assign a penalty "1" when mis-classifying Ytrue =0 and a penality "2.5" when mis-classifying Ytrue =1 

Yval <- datatest[14]; 
valpred <- predict(logitmodel1, datatest, type="response"); 
cutoff1 <- seq(0,1,0.001); 
NN1 <- length(cutoff1);
valerr1 <- NULL;
for (i in 1:NN1) {
  Yvalpred <- (valpred >= cutoff1[i]);
  type1err <- sum( Yval==0 & Yvalpred == 1) / sum(Yval==0);
  type2err <- sum( Yval==1 & Yvalpred == 0) / sum(Yval==1);
  valerr1 <- rbind(valerr1, cbind(type1err,type2err));
}
plot(cutoff1, valerr1[,1],"l", xlab = "Cutoff", ylab = "Error");
lines(cutoff1, valerr1[,2],"l", col="red");

w0 = 1;
w1 = 2.5;
flagoptval <- which.min(w0*valerr1[,1] + w1*valerr1[,2]); 
cutoffoptval <- cutoff1[flagoptval];
cutoffoptval # 0.285

Ypred4 <- (testpred >= cutoffoptval)
table(Ytrue,  Ypred4) 
mean( Ytrue != Ypred4)
# Error Rate = 0.16

# In method (iv), we give high penalty for mis-classifying Ytrue = 1 because we want to make sure that no patient having actual disease should be classified as healthy.
# That means we want to reduce type 2 error



## Deciding a cutoff value for the response variable in logitmodel2

Ytrue <- t(datatest[14])

testpred2 <- predict(logitmodel2, datatest, type="response"); 
testpred2


# (i) Using 0.5 as cutoff value
Ypred1 <- (testpred2 >= 0.5)
table(Ytrue, Ypred1)
mean(Ytrue != Ypred1)
# Error Rate = 0.1733


#(ii) we may choose the cutoff as  the proportion of Y=1 from the training data
Ytrain <- datatrain[14]
cutoffemp <- sum(Ytrain == 1)/dim(Ytrain)[1]
cutoffemp  # 0.5175

Ypred2 <- (testpred2 >= cutoffemp)
table(Ytrue,  Ypred2) 
mean( Ytrue != Ypred2)
# Error Rate = 0.1733


#(iii) Find the best cutoff from the training data
Ytrain <- datatrain[14]
trainpred2 <- predict(logitmodel2, datatrain, type="response") 

cutoff1 <- seq(0,1,0.001)
NN1 <- length(cutoff1);
trainerr1 <- NULL;
for (i in 1:NN1) trainerr1 <- c(trainerr1, mean( Ytrain != (trainpred2 >= cutoff1[i])) );
plot(cutoff1, trainerr1,"l");
flagopt <- which.min(trainerr1); 
cutoffopt <- cutoff1[flagopt];
cutoffopt;  # 0.514

Ypred3 <- (testpred2 >= cutoffopt)
table(Ytrue,  Ypred3) 
mean( Ytrue != Ypred3)
# Error Rate = 0.1733


# (iv) Using validation data to choose cutoff value. 
# Suppose now if we treat the test data as a validation data we assign a penalty "1" when mis-classifying Ytrue =0 and a penality "2.5" when mis-classifying Ytrue =1 

Yval <- datatest[14]; 
valpred <- predict(logitmodel2, datatest, type="response"); 
cutoff1 <- seq(0,1,0.001); 
NN1 <- length(cutoff1);
valerr1 <- NULL;
for (i in 1:NN1) {
  Yvalpred <- (valpred >= cutoff1[i]);
  type1err <- sum( Yval==0 & Yvalpred == 1) / sum(Yval==0);
  type2err <- sum( Yval==1 & Yvalpred == 0) / sum(Yval==1);
  valerr1 <- rbind(valerr1, cbind(type1err,type2err));
}
plot(cutoff1, valerr1[,1],"l");
lines(cutoff1, valerr1[,2],"l", col="red");

w0 = 1;
w1 = 2.5;
flagoptval <- which.min(w0*valerr1[,1] + w1*valerr1[,2]); 
cutoffoptval <- cutoff1[flagoptval];
cutoffoptval # 0.38

Ypred4 <- (testpred >= cutoffoptval)
table(Ytrue,  Ypred4) 
mean( Ytrue != Ypred4)
# Error Rate = 0.1733

# In method (iv), we give high penalty for mis-classifying Ytrue = 1 because we want to make sure that no patient having actual disease should be classified as healthy.
# That means we want to reduce type 2 error

## Decision Tree using rpart library

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
# decision tree using all the parameters
tree = rpart(TARGET~., data=datatrain, method='class')
prp(tree)

predictTree = predict(tree, newdata=datatest, type='class')
table(datatest$TARGET, predictTree)
#Error 0.1866
(9+5)/nrow(datatest)

# decision tree using relevant the parameters 
tree1 = rpart(TARGET~SEX + CP + TRESTBPS + THALACH + EXANG + OLDPEAK + SLOPE + CA + THAL, data=datatrain, method='class')
prp(tree1)

predictTree1 = predict(tree1, newdata=datatest, type='class')
table(datatest$TARGET, predictTree1)
#Error 0.21333
(7+9)/nrow(datatest)


## K-nearest neighbors 

install.packages("class")
library("class")
install.packages("gmodels")
library("gmodels")

pred <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 1)
CrossTable(x = datatest$TARGET, y = pred, prop.chisq = FALSE)
#error 0.48 with k=1

pred2 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 5)
CrossTable(x = datatest$TARGET, y = pred2, prop.chisq = FALSE)
#error 0.40 with k=5

pred3 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 10)
CrossTable(x = datatest$TARGET, y = pred3, prop.chisq = FALSE)
#error 0.466 with k=10

pred4 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 8)
CrossTable(x = datatest$TARGET, y = pred4, prop.chisq = FALSE)
#error 0.427 with k=8

pred5 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 7)
CrossTable(x = datatest$TARGET, y = pred5, prop.chisq = FALSE)
#error 0.413 with k=7

pred5 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 4)
CrossTable(x = datatest$TARGET, y = pred5, prop.chisq = FALSE)
#error 0.386 with k=4

pred5 <- knn(train = datatrain[1:13], test = datatest[1:13], cl = datatrain$TARGET, k = 3)
CrossTable(x = datatest$TARGET, y = pred5, prop.chisq = FALSE)
#error 0.40 with k=3

install.packages('caret')
library(caret)
library(class)
i=1
k.optm=1
for (i in 1:28){
  + knn.model <- knn(train=datatrain[1:13], datatest[1:13], cl=datatrain$TARGET, k=i)
  + k.optm[i] <- 100 * sum(datatest$TARGET == knn.model)/NROW(datatest$TARGET)
  + k=i
  + cat(k,'=',k.optm[i],'')
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
## Lowest error=0.386 occurs when k=4

head(datatrain)
head(datatrain[-c(1,5,6,7)])

## model without AGE, CHOL, FBS, RESTECG

predred <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 1)
CrossTable(x = datatest$TARGET, y = predred, prop.chisq = FALSE)
#error 0.466 with k=1

predred1 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 5)
CrossTable(x = datatest$TARGET, y = predred1, prop.chisq = FALSE)
#error 0.40 with k=5

predred2 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 10)
CrossTable(x = datatest$TARGET, y = predred2, prop.chisq = FALSE)
#error 0.374 with k=10

predred3 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 12)
CrossTable(x = datatest$TARGET, y = predred3, prop.chisq = FALSE)
#error 0.356 with k=12

predred4 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 15)
CrossTable(x = datatest$TARGET, y = predred4, prop.chisq = FALSE)
#error 0.333 with k=15

predred5 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 20)
CrossTable(x = datatest$TARGET, y = predred5, prop.chisq = FALSE)
#error 0.374 with k=20

predred6 <- knn(train = datatrain[-c(1,5,6,7)], test = datatest[-c(1,5,6,7)], cl = datatrain$TARGET, k = 16)
CrossTable(x = datatest$TARGET, y = predred6, prop.chisq = FALSE)
#error 0.356 with k=16

## ## Lowest error=0.333 occurs when k=15

