#!/usr/bin/Rscript
#  mix_elastic_application.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.06.2018

## Example application where data are generated from a mixture model, and the number of clusters is varied in the frontend.

source('web_wrapper.R')
require(mixtools)

## Generate the Data
source('./examples/norm_mix_data.R')

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

# Color each point by its most probable cluster
mixture_col <- function(fit, X, y) {
    clust <- unname(sapply(1:nrow(fit$posterior), 
                           function(i) which.max(fit$posterior[i,])))
    col_types <- c('red', 'blue', 'green', 'orange', 'purple', 'yellow', 'cyan')
    return(col_types[clust])
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'K' = list('dispname' = 'Number of Clusters', 
                               'type' = 'integer', 
                                'min' = 2, 'default' = 2, 'max' = 10)
                    )

set.seed(123)
remote_run(hyperparams = hyperparams, X = X, get_model_fit = fit_mixture,
           get_lat_rep = mixture_lat_rep, get_col = mixture_col)

