#!/usr/bin/Rscript
#  elasticnet_application.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.03.2018

## An example application of webhyperoptim using data generated from a logistic regression and fit using elasticnet via the 'penalized' package

source('web_wrapper.R')

require(penalized)

#Generate some logistic regression data
set.seed(1234)
n <- 200
p <- 100
X <- matrix(rnorm(n*p), ncol = p)
beta <- rnorm(p+1)
y <- as.numeric(pnorm(cbind(1, X) %*% beta + rnorm(n)) > 0.5)

fit <- penalized(y, X, lambda1 = 10)
predict(fit, X)

remote_run(X = X, y = y, get_model_fit = NULL, get_lat_rep = elasticnet_lat_rep,
           get_err = get_ae)
