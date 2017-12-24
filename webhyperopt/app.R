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
                  label = "Number of bins:",
                  min = 0,
                  max = 20,
                  step = 0.1,
                  value = 1),

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "lambda2",
                  label = "Number of bins:",
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

      #For now, do an in-sample fit #TODO: OoS errors
      fit <- penalized(y, X, lambda1=input$lambda1, lambda2=input$lambda2, 
                       model = 'logistic')#TODO: Generalize fitting and prediction
      err <- abs(y - predict(fit, X))

      #Get the latent representation of the input space #TODO: Generalize this
      lat_rep <- X %*% diag(coef(fit, which = 'all')[-1])

      #Do dimensionality reduction on the original data #TODO: Not this
      to_plot <- data.frame(prcomp(X)$x[,1:2])
      to_plot$Error <- err
      p <- ggplot(to_plot, aes(x=PC1, y=PC2, col=Error)) + geom_point() + 
            scale_colour_gradient(low = "black", high = "red") + 
            ggtitle(paste('Visual Representation of Data in Fit (Mean Error of ',
                          mean(err), ')', sep = ' '))
      p

  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
