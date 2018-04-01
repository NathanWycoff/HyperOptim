#!/usr/bin/Rscript
#  sim_nnet.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.12.2018

## Simulate from a neural net, then see if we can gain any insight from visualizing it

#Define some parameters
npm <- 100#Nodes per layer
l <- 2#True number of layers
in_size <- 1e3#Size of the feature space
out_size <- 4#How many categories?
n <- 1e4#Number of observaitons
link <- function(i) plogis(i)#The neural net link function
err_sig <- 1e-1#The error introduced at each layer

#Create the weights at each layer
set.seed(123)
l_sizes <- c(in_size, rep(npm, l), out_size)
W <- lapply(1:(length(l_sizes)-1), 
            function(i) matrix(rnorm(l_sizes[i]*l_sizes[i+1]), 
                               nrow = l_sizes[i], ncol = l_sizes[i+1]))
bias <- lapply(2:length(l_sizes), 
            function(i) rnorm(l_sizes[i])) 

#Create some input, and some output
nn_X <- matrix(rnorm(n*in_size), ncol = in_size)
nn_y <- apply(Reduce(function(inp, i) {
                 Z <- inp %*% W[[i]]
                 B <- matrix(rep(bias[[i]], nrow(Z)), 
                                  ncol = length(bias[[i]]), byrow = T)
                 E <- matrix(rnorm(prod(dim(Z))), nrow = nrow(Z), ncol = ncol(Z))
                 Z <- link(Z + B)
                 return(Z)
                 }, 1:length(W), nn_X), 1, which.max)

save(nn_X, nn_y, file = 'sim_nn.RData')
