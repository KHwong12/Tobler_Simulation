#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)

source("R/tobler.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),

  # Application title
  titlePanel("Tobler's hiking function"),
  
  fluidRow(
    
    withMathJax(),
    
    column(3,
           helpText	("Tobler's hiking function is an exponential function determining the hiking speed, taking into account the slope angle. It was formulated by Waldo Tobler. This function was estimated from empirical data of Eduard Imhof."),
           uiOutput("equation"),
           sliderInput(
             inputId = "speed",
             label = "Walking speed on flat surface (km/hr)",
             min = 0.1, max = 8, step = .1,
             value = 5
           )
    ),
    
    column(9,
           plotlyOutput("toblerPlot")
    )
  ),
  
  hr(),
  
  h2("Examples"),
  
  fluidRow(
    column(3,
           h4("100m path, flat surface"),
           p("The walking time will be:"),
           textOutput(outputId = "eg1_uphill"),
           textOutput(outputId = "eg1_downhill"),           
           ),
    
    column(3,
           h4("40m path, 2.86° slope"),
           p("The walking time will be:"),
           textOutput(outputId = "eg2_uphill"),
           textOutput(outputId = "eg2_downhill"),             
    ),
    
    column(3,
           h4("25m, 30° slope"),
           p("The walking time will be:"),
           textOutput(outputId = "eg3_uphill"),
           textOutput(outputId = "eg3_downhill"),   
    ),
    
    
    column(3,
           h4("What about..."),
           sliderInput(
             inputId = "custom_length",
             label = "Path length (m)",
             min = 10, max = 400, step = 5,
             value = 100
           ),
           sliderInput(
             inputId = "custom_slope",
             label = "Slope (°)",
             min = 0, max = 45, step = .5,
             value = 5
           ),
           
           textOutput(outputId = "custom_uphill"),
           textOutput(outputId = "custom_downhill"),  
           )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {
  
  
  output$toblerPlot <- renderPlotly({
    
    req(input$speed)
    
    speed_flat_terrain <- toblers_hiking_function(0, flat_terrain_speed = input$speed)

    dummydata <- data.frame(slope = seq(-45, 45, by = .2)) %>%
      mutate(speed = toblers_hiking_function(slope_angle = slope, flat_terrain_speed = input$speed))

    tobler_plot <- ggplot(dummydata, aes(x = slope, y = speed)) +
      geom_line() +
      geom_hline(yintercept = speed_flat_terrain, color = "red") +
      ylim(0, 10) +
      theme_minimal() +
      labs(
        x = "Slope of the path (°)",
        y = "Walking speed (km/hr)"
      )


    # should use geom_function instead
    # TODO: how to add shiny interactive input to geom_function()?
    # ggplot() + xlim(-60, 60) +
    #     geom_function(fun = toblers_hiking_function, n = 600)


    ggplotly(tobler_plot)
  })
  
  # TODO: possibly use lapply would be more efficient
  # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761
  
  
  # Render equation
  # double backslash required to avoid being truncated
  # https://shiny.rstudio.com/gallery/mathjax.html
  
  output$equation <- renderUI({
    withMathJax(
      helpText("$$ W = Me^{-3.5 \\times \\left\\lvert tan \\theta + 0.05 \\right\\rvert} $$")
    )
  })
  
  # Render walking time
  
  output$eg1_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(input$speed) / 3.6, 100)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg1_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(input$speed) / 3.6, 100)
    paste("Downhill:", round(time, 2), "s.")
  })

  output$eg2_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(2.86, input$speed) / 3.6, 40)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg2_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(-2.86, input$speed) / 3.6, 40)
    paste("Downhill:", round(time, 2), "s")
  })  

  output$eg3_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(30, input$speed) / 3.6, 40)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg3_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(-30, input$speed) / 3.6, 40)
    paste("Downhill:", round(time, 2), "s")
  })
  
  output$custom_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(input$custom_slope, input$speed) / 3.6, input$custom_length)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$custom_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(-input$custom_slope, input$speed) / 3.6, input$custom_length)
    paste("Downhill:", round(time, 2), "s")
  })  
  
}

# Run the application
shinyApp(ui = ui, server = server)
