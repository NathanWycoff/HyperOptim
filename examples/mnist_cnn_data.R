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
