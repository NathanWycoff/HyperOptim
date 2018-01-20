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
      if (!is.null(y)) {
          #Calculate some things real quick
          n_folds <- 5
          n <- nrow(X)
          folds <- cut(1:n, breaks = n_folds, labels = FALSE)
          err <- c()
          for (i in 1:n_folds) {
              #Train test split
              test_inds <- which(folds == i)
              X_test <- X[test_inds,]
              y_test <- y[test_inds]
              X_train <- X[-test_inds,]
              y_train <- y[-test_inds]

              #Fit the model #TODO: Generalize fitting 
              fit <- get_model_fit(hypers_2_pass, X_train, y_train)

              #Evaluate the fit out of sample
              y_pred <- get_predict(fit, X_test)
              err <- c(err, get_err(y_test, y_pred))
          }
      }

      #Fit the model on all the data#TODO: Generalize fitting 
      if (!is.null(y)) {
          fit <- get_model_fit(hypers_2_pass, X, y)
      } else {
          fit <- get_model_fit(hypers_2_pass, X)
      }

      #Get the latent representation of the input space 
      lat_rep <- get_lat_rep(X, fit)

      #Do dimensionality reduction on the original data 
      to_plot <- data.frame(prcomp(lat_rep)$x[,1:2])
      if (exists('err')) {
          to_plot$Error <- err
      } else {
          to_plot$Error <- 0.5
      }

      p <- ggplot(to_plot, aes(x=PC1, y=PC2, col=Error)) + geom_point() + 
            scale_colour_gradient(low = "black", high = "red", limits = c(0,1)) 

      if (!is.null(y)) {
            p <- p + ggtitle(paste('Visual Representation of Data in Fit (Mean Error of ',
                          mean(err), ')', sep = ' '))
      } else {
            p <- p + ggtitle('Visual Representation of Data in Fit')
      }
      p
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
