#!/usr/bin/Rscript
#  lda_std_example.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.05.2018

# Generate data from the standard LDA model
require(LdaFuncs)
require(mds.methods)
require(entropy)
source('web_wrapper.R')

#Generate data from the LDA model
set.seed(12345)
K <- 40#sample(1:10, 1)
V <- 1000
M <- 500
N.mu <- 100
eta <- 0.1
alpha <- 0.1
gen_result <- gen.lda(K, V, M, N.mu, eta, alpha)
X <- gen_result$docs

#Estimate using the truth
fit_lda <- function(hyperparams, X) {
    fit <- wLDA(X, alpha, eta, hyperparams$K, V, thresh = 5e-6, iters = 1e4)
    fit$K <- hyperparams$K#TODO: This should be done in my LDA package, not here.
    #print(good.dist(fit$BETA, KL.plugin))
    return(fit)
}

# New representation of the data by subtracting its cluster's in the topic space
lda_lat_rep <- function(X, fit) {
    ### Get cluster assignments
    #clusts <- unname(sapply(1:nrow(fit$GAMMA), 
    #                       function(i) which.max(fit$GAMMA[i,])))

    #Mc <- do.call(rbind, lapply(1:fit$K, function(k) colMeans(fit$GAMMA[clusts==k,])))
    #Gc <- fit$GAMMA - Mc[clusts,]

    ##Create a centering matrix, and center the data
    #return(Gc)
    return(fit$BETA)
}

# Color each point by its most probable topic
topic_col <- function(fit, X, y = NULL) {
    #clusts <- unname(sapply(1:nrow(fit$GAMMA), 
    #                       function(i) which.max(fit$GAMMA[i,])))
    #col_types <- palette() 
    #return(col_types[clusts %% length(col_types) + 1])
    return('black')
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'K' = list('dispname' = 'Number of Topics', 
                               'type' = 'integer', 
                                'min' = 2, 'default' = 40, 'max' = 100)
                    )

remote_run(hyperparams = hyperparams, X = X, get_model_fit = fit_lda,
           get_lat_rep = lda_lat_rep, get_col = topic_col)
