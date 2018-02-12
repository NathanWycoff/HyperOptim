library(shiny)
require(ggplot2)

##TODO: This should be replaced by a package load once this is a package
source('frontend_funcs.R')

#Would love to not have to write data to disk and then reread it
load('hyperopt_data.RData')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Visual Hyperparameter Optimization"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # make_inputs creates a sideBarPanel object with the appropriate inputs
    make_inputs(hyperparams),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

# Server logic to fit and visualize the model with the user specified hyperparams
server <- function(input, output) {

  output$distPlot <- renderPlot({
      ## Set the hyperparameters to their new values.
      for (i in 1:length(hyperparams)) {
          hyperparams[[i]][['value']] <- input[[as.character(i)]]
      }

      #Prepare hyperparams for the fitting function
      hypers_2_pass <- lapply(hyperparams, function(i) i$value)

      ### Out of sample error calculation; 5-fold CV#TODO: Generalize
      ## Only used for supervised models
      #if (!is.null(y)) {
      #    #Calculate some things real quick
      #    n_folds <- 5
      #    n <- nrow(X)
      #    folds <- cut(1:n, breaks = n_folds, labels = FALSE)
      #    err <- c()
      #    for (i in 1:n_folds) {
      #        #Train test split
      #        test_inds <- which(folds == i)
      #        X_test <- X[test_inds,]
      #        y_test <- y[test_inds]
      #        X_train <- X[-test_inds,]
      #        y_train <- y[-test_inds]

      #        #Fit the model #TODO: Generalize fitting 
      #        fit <- get_model_fit(hypers_2_pass, X_train, y_train)

      #        #Evaluate the fit out of sample
      #        y_pred <- get_predict(fit, X_test)
      #        err <- c(err, get_err(y_test, y_pred))
      #    }
      #}

      #Fit the model on all the data#TODO: Generalize fitting 
      if (!is.null(y)) {
          fit <- get_model_fit(hypers_2_pass, X, y)
      } else {
          fit <- get_model_fit(hypers_2_pass, X)
      }
      #Get the latent representation of the input space 
      lat_rep <- get_lat_rep(gen_subset(X, to_disp), fit)

      #Do dimensionality reduction on the original data 
      to_plot <- get_2d(lat_rep)
      to_plot <- data.frame(to_plot)
      colnames(to_plot) <- c('X1', 'X2')

      cols <- get_col(fit, gen_subset(X, to_disp), y[to_disp])
      if (class(cols) == class(character(0))) {
          #Make the graph if the color scale is discrete
          to_plot$Color <- cols
          p <- ggplot(to_plot, aes(x=X1, y=X2, col=Color)) + geom_point()
      } else if (class(cols) == class(numeric(0))) {
          #Make the graph if the color scale is continuous
          to_plot$Color <- cols
          p <- ggplot(to_plot, aes(x=X1, y=X2, col=Color)) + geom_point(size=2) + 
                scale_colour_gradient(low = scale_vals[1], high =scale_vals[2]) 
      } else {
          stop("get_col ought to return a vector of length n of either characters or numbers")
      }
      p <- p + ggtitle('Visual Representation of Data in Fit') + coord_fixed()

      p
  }, width = 500, height = 500)
}

# Start the actual app.
shinyApp(ui = ui, server = server)
