#!/usr/bin/Rscript
#  web_wrapper.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 12.24.2017

require(shiny)

#' Run the hyperoptim webserver
#'
#' Start the server for the hyperparameter optimization web gui to visualize fit of a Machine Learning algorithm on data.
#' @param hyperparams The hyperparameters associated with the machine learning model. Should be a list of named lists, each sublist representing one hyperparameter. The name of each sublist will be provided to get_model_fit. All sublists should contain a character element $dispname indicating the display name of the hyperparameter and $type which should be one of "continuous", "integer", and "categorical". A continuous hyperparameter can also contain elements "min", the minimum allowable value, which defaults to -Inf, "max", the maximum allowable value, which defaults to +Inf, "step", a natural increment/decerment amount, and "default", the value at initialization, which defaults to 0, or whichever of max and min prevent it from defaulting to 0. An integer parameter has the same elements, but "min" defaults to 0 and "default" defaults to 1 ("max" still defaults to +Inf), and "step" is fixed to 1. A categorical hyperparameter must have a "categories" element, a character vector representing all allowable values.
#' @param X The "inputs"/"features"/"regressors" of the machine learning model.
#' @param y The "outputs"/"response"/"regressand" of the machine learning model. If left empty, this functions assumes the model is unsupervised.
#' @param get_model_fit A function of the form 'get_model_fit(hyperparams,X,y)' that returns a fitted model object defined by 'hyperparams' given input data X and a response y. 'hyperparams' will be a list containing elements of the same name as the sublists passed to hyperparams. get_predict should know what to do with this, as should the 'get_lat_rep' function also passed. 
#' @param get_lat_rep A function of the form 'get_lat_rep(X, fit)', which takes the input data X and fit, the result of calling 'get_model_fit(X,y)', and returns a latent representation of the data under that model fit. Defaults to the identity function on its first parameter, just returning X.
#' @param get_err A function of the form 'get_err(y_true, y_pred)' which, given a vector y_true containing true outcomes and a corresponding vector of equal length y_pred containing predictions for those values will return a scalar indicating how 'good' the predictions were in some sense. For built in options, see mae_func for continuous data. 
#' @param get_predict A function of the form 'get_predict(fit, X)', which takes a fitted model object from 'fit <- get_model_fit(X,y)' as well as locations at which to predict X, a matrix, and returns predictions of length nrow(X). Defaults to R's 'predict' generic.
#' @param get_col A function of the form 'get_col(fit, X, y = NULL)' which takes a fitted model object, as well as data (y will be passed even in unsupervised mode) and returns either 1) a numeric vector of length nrow(X) with color values to be passed to a scale (in which case col_scale and scale_vals must also be specified), or 2) a character vector of color names (in which case col_scale and scale_vals are ignored).
#' @param col_scale A numeric tuple of the form c(low, high) with the low and high value of the color scale. Defaults the minimum and maximum values returned by 'get_col'
#' @param scale_vals A character tuple of the form c(col1, col2) specifying the extreme values of the color scale.
#' @param max_disp A scalar numeric indicating how many points should be on the scatterplot at once. If nrow(X) is greater than this value, get_lat_rep will be passed a random sample of X's rows instead of the full X object. This sample remains the same throughout a session, but may change between sessions. get_col will also be passed this subset, but other functions, such as get_model_fit, will still see the full X object.
#' @return Does not return a value, is used for its side effect of starting a webserver
remote_run <- function(hyperparams, X, y, get_model_fit, get_lat_rep, get_err, 
                       get_predict = predict, get_col = NULL, 
                       col_scale = c('black', 'red'), scale_vals = NULL,
                       max_disp = 3e3) {

    #Set param defaults
    if (missing(get_lat_rep)) {
        get_lat_rep <- function(X, fit) X
    }
    #Set up for unsupervised models
    if (missing(y)) {
        y <- NULL
        get_err <- NULL
        get_predict <- NULL
    }
    #Default color is just for everything to be black.
    if (is.null(get_col)) {
        get_col <- function(fit, X, y) 'black'
    }

    ## Parse Hyperparameters
    for (i in 1:length(hyperparams)) {
        param <- hyperparams[[i]]
        if (param$type == 'continuous') {
            #Detect the values that have been set
            can_have <- c('min', 'max', 'default', 'step')
            defaults <- list('min' = -Inf, 'max' = Inf, 'default' = 0, 'step' = NA)
            does_have <- can_have %in% names(param)

            #Set default values
            for (toset in can_have[!does_have]) {
                param[[toset]] <- defaults[[toset]]
            }
        } else if (param$type == 'integer') {
            #Detect the values that have been set
            can_have <- c('min', 'max', 'default')
            defaults <- list('min' = 0, 'max' = Inf, 'default' = 1, 'step' = 1)
            does_have <- can_have %in% names(param)

            #Set default values
            for (toset in can_have[!does_have]) {
                param[[toset]] <- defaults[[toset]]
            }
        } else if (param$type == 'categorical') {
            stop("not yet implemented")#TODO: Implement
        } else {
            if (!is.null(param$dispname)) {
                stop(paste('Unrecognized \'type\', should be one of \'continuous\', 
                           \'integer\', or \'categorical\' in hyperparam', 
                           param$dispname))
            } else {
                stop('Unrecognized \'type\', should be one of \'continuous\', 
                     \'integer\', or \'categorical\' in parameter without dispname')
            }
        }

        #Initialize it at its default value, and give it back to the list
        param$value <- param$default
        hyperparams[[i]] <- param
    }

    #Figure out which points we're going to use for the latent representation.
    if (nrow(X) > max_disp) {
        to_disp <- sample(1:nrow(X), max_disp)
        cat('max_disp less than nrow(X), displaying sample of X matrix only\n')
    } else {
        to_disp <- 1:nrow(X)
    }

    #Save the user's specifications to a file in the webserver's directory
    save(hyperparams, X, y, get_model_fit, get_lat_rep, get_err, get_predict, get_col,
         col_scale, scale_vals, to_disp,
         file='./webhyperopt/hyperopt_data.RData')

    #This function starts the actual webserver
    runApp('./webhyperopt/')
}

#' The squared error function.
#'
#' Give the squared error of predictions given the truth. Probably makes most sense to use on continuous data.
#' @param y_true The true response values, a numeric vector.
#' @param y_pred The predictions corresponding to y_true.
#' @return A vector of the same length as the inputs giving the squared error for each.
get_se <- function(y_true, y_pred) {
    #Check that the inputs are of the correct type.
    if (!(is.numeric(y_true) && is.numeric(y_pred))) {
        stop("get_mse is only appropriate for numeric data.")
    }
    #Check that the lengths are equal
    if (!(length(y_true) == length(y_pred))) {
        stop("y_true and y_pred ought to be of the same length.")
    }

    return((y_true - y_pred)^2)
}

#' The absolute error function.
#'
#' Give the absolute error of predictions given the truth. Probably makes most sense to use on continuous data, but can be reasonable for data taking values, e.g. 0 and 1 only.
#' @param y_true The true response values, a numeric vector.
#' @param y_pred The predictions corresponding to y_true.
#' @return A vector of the same length as the inputs giving the absolute error for each.
get_ae <- function(y_true, y_pred) {
    #Check that the inputs are of the correct type.
    if (!(is.numeric(y_true) && is.numeric(y_pred))) {
        stop("get_mae is only appropriate for numeric data.")
    }
    #Check that the lengths are equal
    if (!(length(y_true) == length(y_pred))) {
        stop("y_true and y_pred ought to be of the same length.")
    }

    return(abs(y_true - y_pred))
}
