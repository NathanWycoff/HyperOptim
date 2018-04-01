#!/usr/bin/Rscript
#  gen_lda.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.21.2018

## PreGenerate enough data for 100 participants to do LDA
require(mixtools)

# Set some parameters
K_range <- c(3,10)#range of topics generated for each individual
par_count <- 100#The number of participants

target_dir <- './normmix_data/'

#Generate form a normal mixture model with a specified number of components, with all other parameters randomly generated.
gen_normmix <- function(K) {
    n <- 500#Number of total data
    p <- 4#Number of dimesions

    #Get the mixture weights
    Pi <- rgamma(K, 10, 10) 
    Pi <- Pi / sum(Pi)

    #Simulate from each cluster
    X <- c()
    mus <- c()
    for (k in 1:K) {
        nk <- floor(n*Pi[k])
        mu <- rnorm(p,0,6)
        muk <- matrix(rep(mu, nk), ncol = p, byrow = TRUE)
        Xk <- muk + matrix(rnorm(nk*p), ncol = p)
        #yk <- as.numeric(pnorm(cbind(1, Xk) %*% beta + rnorm(nrow(Xk))) > 0.5)
        X <- rbind(X, Xk)
        mus <- rbind(mus, mu)
    }


    return(X)
}

# Set a seed for reproducible results
set.seed(12345)

true_Ks <- c()
for (par in 1:par_count) {
    #Randomly pick a number of topics
    K <- sample(K_range[1]:K_range[2], 1)

    #Create a directory for this participant
    if (dir.exists(paste(target_dir, 'par_', par, sep = ''))) {
        stop("The directory structure I'm trying to create seems to already exist...")
    }
    dir.create(paste(target_dir, 'par_', par, sep = ''))

    #Generate our data
    X <- gen_normmix(K)

    #Save the truth to memory
    true_Ks <- c(true_Ks, K)

    for (K in K_range[1]:K_range[2]) {
        #Initialize with K means
        km <- kmeans(X, K)
        #mu <- km$centers
        #mu <- lapply(1:K, function(i) mu[i,])
        #SIGMAS <- lapply(1:K, function(i) diag(apply(X[km$cluster==i,], 2, var)))
        #lambda <- km$size / sum(km$size)

        save(km, file = paste(target_dir, 'par_', par, '/fit_', K, '.RData', sep = ''))
    }
}

#Save the truth to file
save(true_Ks, file = paste(target_dir, 'true_Ks.RData', sep = ''))
