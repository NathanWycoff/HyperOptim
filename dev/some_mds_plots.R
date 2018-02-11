#!/usr/bin/Rscript
#  lda_std_example.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.05.2018

## A proof of concept for the LDA viz method of projecting the estimated topics
## and coloring the topics based on their similarity to the uniform topic

# Generate data from the standard LDA model
require(LdaFuncs)
require(mds.methods)
require(entropy)
source('web_wrapper.R')

#Generate data from the LDA model
set.seed(123456)

K_trues <- c(20, 40, 60, 80)
for (K_true in K_trues) {
    V <- 1000
    M <- 500
    N.mu <- 100
    eta <- 0.1
    alpha <- 0.1
    gen_result <- gen.lda(K_true, V, M, N.mu, eta, alpha)
    X <- gen_result$docs

    #Prepare to plot
    rbPal <- colorRampPalette(c('red','blue'))
    br <- 10

    K_ests <- c(20, 40, 60, 80)
    png(paste('./images/lda_output_', K_true, '_euc.png', sep = ''))
    par(mfrow=c(2,2))
    for (K_est in K_ests) {
        fit <- wLDA(X, alpha, eta, K_est, V, thresh = 5e-7, iters = 1e4)
        #low_d <- prcomp(fit$BETA)$x[,1:2]
        low_d <- smacof_forward_mds(fit$BETA, weights = rep(1, V), 
                                      dist.func = euclidean.dist)$par
        #Get KL-div to uniform vector
        cols = sapply(1:K_est, function(i) KL.plugin(fit$BETA[i,], rep(1/V, V)))
        cols <- rbPal(br)[as.numeric(cut(cols,breaks = br))]
        plot(data.frame(low_d), main = 
             paste('K est  = ', K_est, '; K true = ', K_true, sep = ''), col = cols)
    }
    dev.off()
}
