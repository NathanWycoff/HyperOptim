#!/usr/bin/Rscript
#  mix_elastic_application.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.06.2018

## Example application where data are generated from a mixture model, and the number of clusters is varied in the frontend.

source('web_wrapper.R')
require(mixtools)

#Generate some logistic regression data
set.seed(123)
K <- 6#Number of clusters
n <- 400#Number of total data
p <- 4#Number of dimesions

#Get the mixture weights
Pi <- rgamma(K, 10, 10) 
Pi <- Pi / sum(Pi)

#Simulate from each cluster
X <- c()
mus <- c()
for (k in 1:K) {
    nk <- floor(n*Pi[k])
    mu <- rnorm(p,0,3)
    muk <- matrix(rep(mu, nk), ncol = p, byrow = TRUE)
    Xk <- muk + matrix(rnorm(nk*p), ncol = p)
    #yk <- as.numeric(pnorm(cbind(1, Xk) %*% beta + rnorm(nrow(Xk))) > 0.5)
    X <- rbind(X, Xk)
    mus <- rbind(mus, mu)
}

#Account for drops due to flooring
n <- nrow(X)

#If we're doing it in 1D or 2D, plot it, otherwise do PCA
if (p == 1) {
    plot(X)
} else if (p==2) {
    plot(X[,1], X[,2])
} else {
    #Visualize it if we want to
    pca <- princomp(X)
    plot(pca$scores[,1], pca$scores[,2])
}

### Define functions for the webserver
fit_mixture <- function(hyperparams, X) {
    fit <- mvnormalmixEM(X, k = hyperparams$K)
    return(fit)
}

# New representation of the data by subtracting its cluster's mean
mixture_lat_rep <- function(X, fit) {
    ## Get cluster assignments
    clust <- unname(sapply(1:nrow(fit$posterior), 
                           function(i) which.max(fit$posterior[i,])))

    #Create a centering matrix, and center the data
    cent <- matrix(sapply(1:n, function(i) fit$mu[[clust[i]]]), ncol = p, byrow = T)
    Xc <- X - cent
    return(Xc)
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'K' = list('dispname' = 'Number of Clusters', 
                               'type' = 'integer', 
                                'min' = 2, 'default' = 2, 'max' = 10)
                    )

remote_run(hyperparams = hyperparams, X = X, get_model_fit = fit_mixture,
           get_lat_rep = mixture_lat_rep)

