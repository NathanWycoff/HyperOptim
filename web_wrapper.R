#!/usr/bin/Rscript
#  web_wrapper.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 12.24.2017

require(shiny)

remote_run <- function(X, y) {
    save(X, y, file='./webhyperopt/hyperopt_data.RData')
    runApp('./webhyperopt/')
}

#Generate some logistic regression data
n <- 200
p <- 100
X <- matrix(rnorm(n*p), ncol = p)
beta <- rnorm(p+1)
y <- as.numeric(pnorm(cbind(1, X) %*% beta + rnorm(n)) > 0.5)

remote_run(X, y)
