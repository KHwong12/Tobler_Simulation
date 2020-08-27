
# Shiny application for simulating Tobler's hiking function

library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(magrittr)

source("R/tobler.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Theme options --------
  theme = shinytheme("flatly"),
  includeCSS("style.css"),

  # Application title ----------
  titlePanel("Tobler's hiking function"),
  
  # Top panel ----------
  
  fluidRow(
    
    withMathJax(),
    
    column(4,
           p("Tobler's hiking function is an exponential function determining the hiking speed, taking into account the slope angle. It was formulated by Waldo Tobler. This function was estimated from empirical data of Eduard Imhof."),
           p("The equation takes the following form:"),
           uiOutput("equation"),
           
           p("This web application lets you to play with the parameters of the equation to see how the walking time changes."),
           
           
           sliderInput(
             inputId = "speed",
             label = "Walking speed on flat surface (km/hr)",
             min = 0.1, max = 8, step = .1,
             value = 5
           )
           
           
    ),
    
    column(8,
           plotlyOutput("toblerPlot")
    )
  ),
  
  hr(),
  
  # Bottom panel ----------
  
  h2("How long does it take?"),
  
  p("With your walking speed given above, how long does take to finish the path with the slope of...?"),
  
  numericInput(
    inputId = "custom_length",
    label = "Path length (m)",
    min = 10, max = 1000, step = 10,
    value = 100,
    width = "20%"
  ),


  fluidRow(
    column(3,
           h4("Flat Terrain"),
           img(src = "https://1.bp.blogspot.com/-wEygfnu5_mc/V5jHkBSzzLI/AAAAAAAA80w/DdLElofgt_Qn8RbZStkfWjnXhIH8n7cpgCLcB/s200/walking_businesswoman.png"),
           p("The walking time will be:"),
           textOutput(outputId = "eg1_uphill"),
           textOutput(outputId = "eg1_downhill"),           
           ),
    
    column(3,
           h4("2.86° Slope"),
           img(src = "https://1.bp.blogspot.com/-59_nvImHVnM/XkZdUFSPVeI/AAAAAAABXWQ/Vbu2acjd6dwZjOoQIhRGeYjKPY2EtUCewCNcBGAsYHQ/s200/yagai_kyoushitsu_casual_walk.png"),
           htmlOutput("eg2_picture"),
           p("The walking time will be:"),
           textOutput(outputId = "eg2_uphill"),
           textOutput(outputId = "eg2_downhill"),             
    ),
    
    column(3,
           h4("20° Slope"),
           img(src = "https://2.bp.blogspot.com/-78mChg3NsLQ/VGLMgDJiciI/AAAAAAAApBk/3zAG9kQK1Fg/s200/noborizaka_saka.png"),
           htmlOutput("eg3_picture"),
           p("The walking time will be:"),
           textOutput(outputId = "eg3_uphill"),
           textOutput(outputId = "eg3_downhill"),   
    ),
    
    
    column(3,
           h4("What about..."),
           sliderInput(
             inputId = "custom_slope",
             label = "Slope (degree)",
             min = 0, max = 45, step = .5,
             value = 5,
             width = "100%"
           ),
           
           p("The walking time will be:"),
           textOutput(outputId = "custom_uphill"),
           textOutput(outputId = "custom_downhill"),  
           )
  ),
  
  hr(),
  
  # Footer ---------------
  div(class = "footer",
      includeHTML("template/footer.html")
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
    
    ggplotly(tobler_plot)

    # should use geom_function instead
    # TODO: how to add shiny interactive input to geom_function()?
    # ggplot() + xlim(-60, 60) +
    #     geom_function(fun = toblers_hiking_function, n = 600)

    # TODO: Manually add second axis, as ggplotly() does not support dual axis
    # https://stackoverflow.com/questions/52833214/adding-second-y-axis-on-ggplotly

  })
  

  # Render equation ------------------
  # double backslash required to avoid being truncated
  # https://shiny.rstudio.com/gallery/mathjax.html
  
  output$equation <- renderUI({
    withMathJax(
      helpText("$$ W = Me^{-3.5 \\times \\left\\lvert tan \\theta + 0.05 \\right\\rvert} $$")
    )
  })
  
  # Render walking time ----------
  # TODO: possibly use lapply would be more efficient
  # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761
  
  # lapply(1:3, function(example_num) {
  #   outputId <- paste0("eg", example_num, "_uphill")
  #   
  #   time <- walking_time(toblers_hiking_function(input$speed) / 3.6, input$custom_length)
  #   
  #   output[[outputId]] <- renderText("Uphill:", round(time, 2), "s")
  #   
  #   
  #   
  # })
  
  output$eg1_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(0, input$speed) / 3.6, input$custom_length)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg1_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(0, input$speed) / 3.6, input$custom_length)
    paste("Downhill:", round(time, 2), "s.")
  })
  
  output$eg2_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(2.86, input$speed) / 3.6, input$custom_length)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg2_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(-2.86, input$speed) / 3.6, input$custom_length)
    paste("Downhill:", round(time, 2), "s")
  })
  
  output$eg3_uphill <- renderText({
    time <- walking_time(toblers_hiking_function(20, input$speed) / 3.6, input$custom_length)
    paste("Uphill:", round(time, 2), "s")
  })
  
  output$eg3_downhill <- renderText({
    time <- walking_time(toblers_hiking_function(-20, input$speed) / 3.6, input$custom_length)
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
