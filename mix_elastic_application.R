#!/usr/bin/Rscript
#  mix_elastic_application.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.06.2018

## An example application of webhyperoptim using data generated from a mixture of regressions and fit using a mixture of elasticnets

source('web_wrapper.R')
source('FLXMRglmnet_no_cv.R')
require(flexmix)
require(penalized)
require(glmnet)

#Generate some logistic regression data
set.seed(123)
K <- 3#Number of clusters
n <- 200#Number of total data
p <- 4#Number of dimesions

#Get the mixture weights
Pi <- rgamma(K, 10, 10) 
Pi <- Pi / sum(Pi)

#Simulate from each cluster
X <- c()
y <- c()
for (k in 1:K) {
    nk <- floor(n*Pi[k])
    muk <- matrix(rep(rnorm(p,0,1), nk), ncol = p, byrow = TRUE)
    Xk <- muk + matrix(rnorm(nk*p), ncol = p)
    beta <- rnorm(p+1)
    #yk <- as.numeric(pnorm(cbind(1, Xk) %*% beta + rnorm(nrow(Xk))) > 0.5)
    yk <- cbind(1, Xk) %*% beta + rnorm(nrow(Xk))
    X <- rbind(X, Xk)
    y <- c(y, yk)
}

#Account for drops due to flooring
n <- nrow(X)

#If we're doing it in 1D, plot it:
if (p == 1) {
    plot(X, y)
}

##For testing purposes only
hyperparams <- list(K=k)

### Define functions for the webserver
#Make a version of that doesn't do CV
# Fit an elastic net with the defined hyperparams
fit_mixture <- function(hyperparams, X, y) {
    #respon <- matrix(0, nrow = n, ncol = 2)
    #for (i in 1:n) {
    #    respon[i, (y[i]==1) + 1] <- 1
    #}

    fit <- flexmix(y ~ X, k = hyperparams$K,
               model = list(FLXMRglm(y ~ X, family= "gaussian")) )

    return(fit)
}

#A mini train-test split just for fun
set.seed(123)
ord <- sample(1:198)
Xs <- X[ord, ]
ys <- y[ord]
X_train <- Xs[1:100,]
X_test <- Xs[101:198,]
y_train <- ys[1:100]
y_test <- ys[1:100]

fit <- fit_mixture(list('K'=3), X_train, y_train)

mean((y_test - predict_mixture(fit, X_test))^2)
mean((y_test - mean(y_train))^2)

predict_mixture <- function(fit, X) {
    preds <- predict(fit, as.data.frame(X))#Get predictions for each cluster
    preds <- do.call(cbind, preds)#Turn it into a matrix
    print('it is here, right?')
    flush.console()
    clusts <- clusters(fit, as.data.frame(X))#Get most likely clusters
    flush.console()
    print('Or not?')
    preds <- sapply(1:nrow(X), function(i) preds[i,clusts[i]])#Get most likely predictions
    return(preds)
}

# New representation of the data by subtracting its cluster's mean
elasticnet_lat_rep <- function(X, fit) {
    #Get cluster means
    clust <- clusters(fit)
    K <- max(clust)
    mus <- sapply(1:K, function(i) colMeans(X[clust==i,]))

    #Create a centering matrix, and center the data
    cent <- matrix(sapply(1:n, function(i) mus[,clust[i]]), ncol = p, byrow = T)
    Xc <- X - cent
    return(Xc)
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'K' = list('dispname' = 'Number of Clusters', 
                               'type' = 'integer', 
                                'min' = 1, 'default' = 1, 'max' = 10)
                    )

remote_run(hyperparams = hyperparams, X = X, y = y, get_model_fit = fit_mixture, 
           get_lat_rep = elasticnet_lat_rep, get_err = get_ae, 
           get_predict = predict_mixture)

