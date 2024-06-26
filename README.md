
<!-- README.md is generated from README.Rmd. Please edit that file -->

# penalizedbeta

<!-- badges: start -->
<!-- badges: end -->

The goal of penalizedbeta is to …

## Installation

You can install the development version of penalizedbeta from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("girelaignacio/penalizedbeta")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(penalizedbeta)
## basic example code

data <- penalizedbeta::data

y <- data$y
X <- data[,-1]

n <- dim(X)[1]; p <- dim(X)[2]

split <- sample(x = c("train","test"), size = n, 
                prob = c(0.8,0.2), replace = TRUE)
Xtrain <- X[split == "train",] ; Xtest <- X[split == "test",]
ytrain <- y[split == "train"] ; ytest <- y[split == "test"]


nlambda = 100
lambdas <-  seq(0.0001,0.1, length.out = nlambda)
mse_train <- matrix(nrow = nlambda, dimnames = list(lambdas,NULL))
mse_test <- matrix(nrow = nlambda, dimnames = list(lambdas,NULL))
id = 0
for (s in lambdas){
  # fit
  fit <- betareg_lasso(Xtrain,ytrain,lambda = s)
  
  # predict
  ypred_train <- predict(fit, newdata = Xtrain)
  ypred_test <- predict(fit, newdata = Xtest)
  
  # mse
  id = id + 1
  mse_train[id] <- mean((ytrain-ypred_train)^2)
  mse_test[id] <- mean((ytest-ypred_test)^2)
  
  if(id %% 10 == 0)print(paste(id,"-th iteration", sep=""))
}
#> [1] "10-th iteration"
#> [1] "20-th iteration"
#> [1] "30-th iteration"
#> [1] "40-th iteration"
#> [1] "50-th iteration"
#> [1] "60-th iteration"
#> [1] "70-th iteration"
#> [1] "80-th iteration"
#> [1] "90-th iteration"
#> [1] "100-th iteration"


mse_train[which.min(mse_train)]
#> [1] 0.2227157
mse_test[which.min(mse_train)]
#> [1] 0.2209785

mse_test[which.min(mse_test)]
#> [1] 0.1552514
mse_train[which.min(mse_test)]
#> [1] 0.3354095

# Fit
betaboost <- mboost::glmboost(ytrain~., data = as.data.frame(cbind(ytrain,Xtrain)), family = betaboost::BetaReg())
ypred_betaboost <- predict(betaboost, type = "response", newdata = Xtest)

mean((ytest-ypred_betaboost)^2)
#> [1] 0.08907387
```
