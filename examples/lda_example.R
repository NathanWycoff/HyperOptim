#!/usr/bin/Rscript
#  lda_std_example.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.05.2018

# Generate data from the standard LDA model
require(LdaFuncs)
require(mds.methods)
require(entropy)
source('web_wrapper.R')

#Generate data from the LDA model
set.seed(12345)
K <- 20#sample(1:10, 1)
V <- 1000
M <- 500
N.mu <- 100
eta <- 0.1
alpha <- 0.1
Pi <- rep(1, V)
gen_result <- gen.lda(K, V, M, N.mu, Pi, eta, alpha)
X <- gen_result$docs

#Estimate using the truth
fit_lda <- function(hyperparams, X) {
    fit <- wLDA(X, alpha, eta, hyperparams$K, V, thresh = 5e-6, iters = 1e4)
    fit$K <- hyperparams$K#TODO: This should be done in my LDA package, not here.
    return(fit)
}

# New representation of the data by subtracting its cluster's in the topic space
lda_lat_rep <- function(X, fit) {
    return(fit$BETA)
}

# Color each point by its most probable topic
topic_col <- function(fit, X, y = NULL) {
    return(sapply(1:fit$K, function(i) KL.plugin(fit$BETA[i,], rep(1/V, V))))
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'K' = list('dispname' = 'Number of Topics', 
                               'type' = 'integer', 
                                'min' = 2, 'default' = 40, 'max' = 100)
                    )

log_name <- paste('lda_log_', as.numeric(Sys.time()), '.txt', sep = '')

remote_run(hyperparams = hyperparams, X = X, get_model_fit = fit_lda,
           get_lat_rep = lda_lat_rep, get_col = topic_col, get_2d = kl_mds, 
           log = TRUE, log_file = paste('/Users/nathw95/', log_name, sep = ''),
           scale_vals = c('red', 'black'))
