#!/usr/bin/Rscript
#  examples/mnist_cnn_data.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.21.2018

## Prepare the MNIST data for training a CNN using Keras
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# rescale
x_train <- x_train / 255
x_test <- x_test / 255

# only keep data where y = 0,1
to_keep_train <- y_train==4 | y_train==7
x_train <-x_train[to_keep_train,]
y_train <-y_train[to_keep_train]
to_keep_test <- y_test==4 | y_test==7
x_test <-x_test[to_keep_test,]
y_test <-y_test[to_keep_test]
