#!/usr/bin/Rscript
#  norm_mix_data.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.21.2018

## Generate the data for the norm_mix example

#Generate some logistic regression data
set.seed(1234)
K <- 5#Number of clusters
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

#Account for drops due to flooring
n <- nrow(X)

##If we're doing it in 1D or 2D, plot it, otherwise do PCA
#if (p == 1) {
#    plot(X)
#} else if (p==2) {
#    plot(X[,1], X[,2])
#} else {
#    #Visualize it if we want to
#    pca <- princomp(X)
#    plot(pca$scores[,1], pca$scores[,2])
#}
#
