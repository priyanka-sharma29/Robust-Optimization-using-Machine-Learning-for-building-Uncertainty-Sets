# We compute the intervals using 3 methods:

# Inputs are X, Y and X_new dataframes & outputs are the prediction intervals.

# Method 0 - Simple OLS regression
method.0 <- function(X, Y, X_new)
{
  model1 <- lm(Y$response ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5, data = X)
  summary(model1)
  predictions <- predict(model1, X_new)
  ols_predictions <- data.frame(actualEmptyDocks = X_new$response, predictedEmptyDocks = predictions)
  return (ols_predictions)
}



# Method 2 - Conditional quantile regression method
method.2 <- function(X, Y, X_new)
{
  library(quantreg)
  model2 <- rq(Y$response ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5, tau = c(0.05, 0.95), data = X)
  summary(model2)
  predictions <- predict(model2, newdata = X_new, interval = "prediction")
  intervals_by_method2 <- data.frame(actualEmptyDocks = X_new$response, predictionIntervals = predictions)
  return (intervals_by_method2);
}



# Method 3 - OLS + Wiggle room method
method.3 <- function(X, Y, X_new)
{
  #Step 1 - Apply multiple regression modeling   
  model3 <- lm(Y$response ~ predictor1 + predictor2 + predictor3 + predictor4 + predictor5, data = X)
  summary(model3)
  
  #Step 2 - If the data has factor variables, we convert it to dummy variables through one-hot encoding
  #install.packages('ade4')
  #install.packages('data.table')
  library(ade4)
  library(data.table)
  ohe_feats <- names(Filter(is.factor, X))
  for (f in ohe_feats){
    df_all_dummy = acm.disjonctif(X[f])
    X[f] = NULL
    X = cbind(X, df_all_dummy)
  }
  #remove period from the column names
  names(X) <- gsub("\\.", "", names(X))
  
  #Step 3 - Checking regression coefficients from Model3
  coeff <- model3$coefficients
  
  #Step 4 - Subset X as per coefficients obtained from linear regression
  names1 <- names(model3$coefficients)
  X1 <- X[,names(X) %in% names(model3$coefficients)]
  
  #Step 5 - Append vector of 1's as the first column in X
  vec1 <- rep(1, length(X$predictor1))
  X2 <- cbind(intercept = vec1, X1)
  X2 <- as.matrix(X2)
  
  #Step 6 - Calculating A as (XtransposeX)inverse
  B <- t(X2) %*% X2
  A <- solve(B)
  options(scipen = 999)

  #Step 7 - Multiplying A by a constant for adding the wiggle room and creating a blank matrix
  constantvalue <- readline(prompt="Enter constant for adding wiggle room: ")
  # convert character into integer
  constantvalue <- as.integer(constantvalue)
  A <- A * constantvalue
  mat1 <- matrix(NA, nrow = nrow(X2) , ncol = 2)
  
  #Step 8 - Calculating min and max
  for(i in 1:nrow(X2))
  {
    row <- X2[i,]
    min = t(row) %*% model3$coefficients - sqrt(t(row) %*% (A %*% row))
    max = t(row) %*% model3$coefficients + sqrt(t(row) %*% (A %*% row))
    mat1[i,] = c(min, max)
  }
  intervals_by_method3 <- cbind(OLSprediction = model3$fitted.values,mat1)
  return (intervals_by_method3);
}

################### check what to do for test data ?!