#!/usr/bin/Rscript
#  gen_lda.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.21.2018

## PreGenerate enough data for 100 participants to do LDA
require(LdaFuncs)

# Set some parameters
K_range <- c(20, 100)#range of topics generated for each individual
par_count <- 100#The number of participants

target_dir <- './lda_data/'

#Params for LDA that are constant
V <- 1000
M <- 500
N.mu <- 100
eta <- 0.1
alpha <- 0.1
Pi <- rep(1, V)

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
    gen_result <- gen.lda(K, V, M, N.mu, Pi, eta, alpha)
    docs <- gen_result$docs

    #Save the truth to memory
    true_Ks <- c(true_Ks, K)

    for (K in K_range[1]:K_range[2]) {
        fit <- wLDA(docs, alpha = 1, eta = 1, K, V, thresh = 5e-6, iters = 1e4)
        save(fit, file = paste(target_dir, 'par_', par, '/fit_', K, '.RData', sep = ''))
    }
}

#Save the truth to file
save(true_Ks, file = paste(target_dir, 'true_Ks.RData', sep = ''))
