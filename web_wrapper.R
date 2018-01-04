#!/usr/bin/Rscript
#  web_wrapper.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 12.24.2017

require(shiny)

#' Run the hyperoptim webserver
#'
#' Start the server for the hyperparameter optimization web gui to visualize fit of a Machine Learning algorithm on data.
#' @param X The "inputs"/"features"/"regressors" of the machine learning model.
#' @param y The "outputs"/"response"/"regressand" of the machine learning model.
#' @param get_model_fit A function of the form 'get_model_fit(X,y)' that returns a fitted model object given input data X and a response y. R's built in 'predict' should know what to do with this, as should the 'get_lat_rep' function also passed. 
#' @param get_lat_rep A function of the form 'get_lat_rep(X, fit)', which takes the input data X and fit, the result of calling 'get_model_fit(X,y)', and returns a latent representation of the data under that model fit.
#' @param get_err A function of the form 'get_err(y_true, y_pred)' which, given a vector y_true containing true outcomes and a corresponding vector of equal length y_pred containing predictions for those values will return a scalar indicating how 'good' the predictions were in some sense. For built in options, see mae_func for continuous data. 
#' @param get_predict A function of the form 'get_predict(fit, X)', which takes a fitted model object from 'fit <- get_model_fit(X,y)' as well as locations at which to predict X, a matrix, and returns predictions of length nrow(X). Defaults to R's 'predict' generic.
#' @return Does not return a value, is used for its side effect of starting a webserver
remote_run <- function(X, y, get_model_fit, get_lat_rep, get_err, 
                       get_predict = predict) {

    #Save the user's specifications to a file in the webserver's directory
    save(X, y, get_model_fit, get_lat_rep, get_err, get_predict, 
         file='./webhyperopt/hyperopt_data.RData')

    #This function starts the actual webserver
    runApp('./webhyperopt/')
}

#' A function which returns a latent representation for GLM data.
#'
#' Suitable in general for models in which there is one model parameter associated with each input dimension: simply scale the input dimension of X by the parameter value.
#' 
#' @param X The "inputs"/"features"/"regressors" of the machine learning model.
#' @param fit A fitted model object, the result of 'get_model_fit' generally, but the result of 'glm' in this case.
glm_lat_rep <- function(X, fit) X %*% diag(coef(fit)[-1])


#' A modificaiton of glm_lat_rep for the penalized function from the penalized pacakge. 
#'
#' @param X The "inputs"/"features"/"regressors" of the machine learning model.
#' @param fit A fitted model object, the result of 'get_model_fit' generally, but the result of 'penalized' in this case.
elasticnet_lat_rep <- function(X, fit) X %*% diag(coef(fit, which = 'all')[-1])

#' The SE error function.
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

#' The AE error function.
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
