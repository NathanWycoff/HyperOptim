library(shiny)
require(ggplot2)
require(penalized)

load('hyperopt_data.RData')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Visual Hyperparameter Optimization"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(


      # Input: Slider for the number of bins ----
      sliderInput(inputId = "lambda1",
                  label = "L1 Regularization Coefficient",
                  min = 0,
                  max = 20,
                  step = 0.1,
                  value = 1),

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "lambda2",
                  label = "L2 Regularization Coefficient",
                  min = 0,
                  max = 20,
                  step = 0.1,
                  value = 1)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$distPlot <- renderPlot({

      ### Out of sample error calculation; 5-fold CV
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

          #Fit the model #TODO: Generalize fitting and prediction
          fit <- penalized(y_train, X_train, lambda1=input$lambda1, 
                           lambda2=input$lambda2, model = 'logistic')
          err <- c(err, abs(y_test - predict(fit, X_test)))
      }

      #Get the latent representation of the input space #TODO: Generalize this
      lat_rep <- X %*% diag(coef(fit, which = 'all')[-1])

      #Do dimensionality reduction on the original data #TODO: Not this
      to_plot <- data.frame(prcomp(X)$x[,1:2])
      to_plot$Error <- err
      p <- ggplot(to_plot, aes(x=PC1, y=PC2, col=Error)) + geom_point() + 
            scale_colour_gradient(low = "black", high = "red", limits = c(0,1)) + 
            ggtitle(paste('Visual Representation of Data in Fit (Mean Error of ',
                          mean(err), ')', sep = ' '))
      p

  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
