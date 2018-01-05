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

### Define functions for the webserver
# Fit an elastic net with the defined hyperparams
fit_elasticnet <- function(hyperparams, X, y) {
    fit <- penalized(y, X, lambda1=hyperparams$l1, 
        lambda2=hyperparams$l2, model = 'logistic')
    return(fit)
}

# Scale the dimensions by their corresponding coefficient
elasticnet_lat_rep <- function(X, fit) X %*% diag(coef(fit, which = 'all')[-1])

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'l1' = list('dispname' = 'L1 Coef', 'type' = 'continuous', 
                                'min' = 0, 'default' = 1, 'max' = 10),
                    'l2' = list('dispname' = 'L2 Coef', 'type' = 'continuous', 
                                'min' = 0, 'default' = 1, 'max' = 10)
                    )

remote_run(hyperparams = hyperparams, X = X, y = y, get_model_fit = fit_elasticnet, 
           get_lat_rep = elasticnet_lat_rep, get_err = get_ae)
