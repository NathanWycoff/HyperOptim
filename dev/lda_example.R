#!/usr/bin/Rscript
#  lda_example.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.04.2018

# See if we can generate clusters on the simplex
require(LdaFuncs)

#Generate dirichlet variates
rdirich <- function(n, alpha) {
    ret <- lapply(1:n, function(i) {
        ret <- rgamma(length(alpha), alpha, 1)
        ret <- ret / sum(ret)
        return(ret)
    })
    return(ret)
}

# Generate some params
set.seed(123)
L <- 4
K <- 10
n.mu <- 50
alphas <- lapply(1:L, function(l) rgamma(K, 0.8, 0.8))
X <- c()
clusts <- c()

# Generate some data
for (l in 1:L) {
    m <- rpois(1, n.mu)
    draws <- rdirich(m, alphas[[l]])
    draws <- do.call(rbind, draws)
    X <- rbind(X, draws)
    clusts <- c(clusts, rep(l, m))
}

cols <- palette()
maj_topics <- sapply(1:nrow(X), function(i) which.max(X[i,]))
col_to_plot <- maj_topics %% length(cols)

par(mfrow=c(1,2))
# Project them
ld <- prcomp(X)
plot(ld$x[,1:2], col = col_to_plot)

# Project again, centering by cluster centroid
Mc <- do.call(rbind, lapply(1:L, function(l) colMeans(X[clusts==l,])))
ld <- prcomp(X - Mc[clusts,])
plot(ld$x[,1:2], col = col_to_plot)
