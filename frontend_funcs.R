#!/usr/bin/Rscript
#  frontend_funcs.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 01.04.2018

##Functions that help create the frontend

require(shiny)


#' Create the input elements based on user-supplied hyperparams
make_inputs <- function(hyperparams) {
    inputs <- list()
    ## For now, assumes inputs are continuous
    #TODO:: not this
    for (i in 1:length(hyperparams)) {
        param <- hyperparams[[i]]
        #Make a slider if both elements are finite
        if (sum(is.finite(c(param$min, param$max)))==2) {
            inputs[[i]] <- sliderInput(inputId = as.character(i),
                  label = param$dispname,
                  min = param$min,
                  max = param$max,
                  value = param$default)
        } else {
            #Make a numercInput if not
            inputs[[i]] <- numericInput(inputId = as.character(i),
                  label = param$dispname,
                  min = param$min,
                  max = param$max,
                  value = param$default)
        }
    }

    #Create the side bar 
    return(do.call(sidebarPanel, inputs))
}
