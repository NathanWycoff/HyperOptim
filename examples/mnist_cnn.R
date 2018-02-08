#!/usr/bin/Rscript
#  dev/mnist_cnn.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.21.2018

## Example on the MNIST data using an Artificial Convolutional Neural Net
require(keras)
source('web_wrapper.R')

source('./examples/mnist_cnn_data.R')

## Fit a neural net on the data
fit_cnn <- function(hyperparams, X, y) {
    # Put the response in the form keras likes
    q <- length(unique(y))
    y <- sapply(y, function(i) which(i==unique(y))-1)
    y <- to_categorical(y, q)

    # Set up the model archetecture
    model <- keras_model_sequential()
    for (l in 1:hyperparams$layers) {
        if (l == 1) {
            model %>%
              layer_dense(units = hyperparams$npl, activation = 'relu', 
                          input_shape = c(784)) %>%
              layer_dropout(rate = 0.4)
        } else {
            model %>%
              layer_dense(units = hyperparams$npl, activation = 'relu') %>%
              layer_dropout(rate = 0.4)
        }
    }
    model %>%
      layer_dense(units = q, activation = 'softmax')

    # Prepare the model for training
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )

    # train the model on the training data
    history <- model %>% keras::fit(
      X, y,
      epochs = 3, batch_size = 128,
      validation_split = 0.2
    )

    return(model)
}

## Get the last hidden layer activations as a latent representation of the images
cnn_lat_rep <- function(X, fit) {
    m <- clone_model(fit)
    pop_layer(m)
    return(predict(m, X))
}

## Simply color by class, this may become a default
cnn_col <- function(fit, X, y) {
    #cols <- rainbow(length(unique(y)))
    return(as.character(y))
}

# We need to define some properties relating to the model hyperparameters
hyperparams <- list(
                    'layers' = list('dispname' = 'Number of Hidden Layers', 
                               'type' = 'integer', 
                                'min' = 1, 'default' = 2, 'max' = 5),
                    'npl' = list('dispname' = 'Nodes Per Layer',
                                 'type' = 'integer',
                                 'min' = -Inf, 'default' = 100, 'max' = Inf)
                    )

remote_run(hyperparams = hyperparams, X = x_train, y = y_train,
           get_model_fit = fit_cnn,
           get_lat_rep = cnn_lat_rep, get_col = cnn_col)
