#!/usr/bin/Rscript
#  viz_nnet.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.12.2018

#Visualize our simulated ann
require(keras)
source('web_wrapper.R')
load('sim_nn.RData')

## Fit a neural net on the data
fit_ann <- function(hyperparams, X, y) {
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
                          input_shape = c(ncol(X))) %>%
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
      epochs = 30, batch_size = 128,
      validation_split = 0.2
    )

    return(model)
}

## Get the last hidden layer activations as a latent representation of the images
ann_lat_rep <- function(X, fit) {
    m <- clone_model(fit)
    pop_layer(m)
    return(predict(m, X))
}

## Simply color by class, this may become a default
ann_col <- function(fit, X, y) {
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
                                 'min' = 10, 'default' = 100, 'max' = 1000)
                    )

remote_run(hyperparams = hyperparams, X = nn_X, y = nn_y,
           get_model_fit = fit_ann,
           get_lat_rep = ann_lat_rep, get_col = ann_col)
