
# Shiny application for simulating Tobler's hiking function

library(shiny)
library(shinythemes)

library(ggplot2)
library(plotly)

library(magrittr)
library(stringr)

library(glue)
library(markdown) # includeMarkdown

source("R/tobler.R")

# Types of input widgets
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Theme options --------
  theme = shinytheme("flatly"),
  includeCSS("style.css"),

  # Head ----------
  titlePanel("Tobler's hiking function"),

  h3("Simulating the effects of terrain slope to walking speed"),

  br(),

  # Top panel ----------
  
  withMathJax(htmltools::includeMarkdown("./md/usage.md")),
  
  br(),

  sliderInput(
    inputId = "speed",
    label = "Walking speed on flat surface (km/hr)",
    min = 0.1, max = 8, step = .1,
    value = 5
  ),
  
  br(),

  plotlyOutput("toblerPlot"),


  hr(),

  # Bottom panel ----------

  h3("How long does it take?"),

  p("With your walking speed given above, what will be the time needed to finish a path?
    Enter a path length below. See how long you need to finish the paths with same length, yet with different slope."),

  numericInput(
    inputId = "custom_length",
    label = "Path length (m)",
    min = 10, max = 1000, step = 10,
    value = 100,
    width = "20%"
  ),

  br(),

  p("The walking time to finish the following paths will be:"),

  fluidRow(
    column(
      3,
      h4("Flat Terrain"),
      img(src = "walk-flat-terrain.png",
          style = "height:150px;")
    ),

    column(
      3,
      h4("2.86° Slope (1:20 Gradient)"),
      img(src = "casual-walk.png",
          style = "height:150px;")
    ),

    column(
      3,
      h4("20° Slope"),
      img(src = "uphill-slope.png",
          style = "height:150px;")
    ),


    column(
      3,
      h4("What about..."),
      
      # 200px height, same as the illustrations
      div(
        style = "height:200px;margin: auto;",

        sliderInput(
        inputId = "custom_slope",
        label = "Slope (degree)",
        min = 0, max = 45, step = .1,
        value = 5,
        width = "80%")
      )
    )
  ),
  
  # computed walking time
  fluidRow(
    column(
      3,
      textOutput(outputId = "eg1_uphill"),
      textOutput(outputId = "eg1_downhill")
    ),
    
    column(
      3,
      textOutput(outputId = "eg2_uphill"),
      textOutput(outputId = "eg2_downhill")
    ),
    
    column(
      3,
      textOutput(outputId = "eg3_uphill"),
      textOutput(outputId = "eg3_downhill")
    ),
    
    
    column(
      3,
      textOutput(outputId = "custom_uphill"),
      textOutput(outputId = "custom_downhill")
    )
  ),

  hr(),

  # Footer ---------------
  div(
    class = "footer",
    includeHTML("template/footer.html")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session, ...) {
  output$toblerPlot <- renderPlotly({
    req(input$speed)

    speed_flat_terrain <- toblers_hiking_function(0, input$speed)

    tobler_data <- data.frame(slope = seq(-50, 50, by = .05)) %>%
      mutate(speed = toblers_hiking_function(slope, input$speed))

    tobler_plot <- ggplot(tobler_data, aes(x = slope, y = speed)) +
      # geom_line(color = "#2c3e50", aes(
      #   text = paste("Slope:",round(slope, 2), "degrees", "\nWalking Speed:", round(speed, 2), "km/hr")
      #   )) +
      geom_line(color = "#2c3e50") +
      geom_hline(yintercept = speed_flat_terrain, color = "#e7298a", linetype = "dotdash") +
      ylim(0, 10) +
      theme_minimal() +
      labs(
        x = "Slope of the path (°)",
        y = "Walking speed (km/hr)",
        title = "Tobler's hiking function – how walking speed is determined by the slope"
      )

    # ggplotly(tobler_plot, tooltip = c("text"))
    # TODO: check usage of tooltip
    # https://plotly-r.com/controlling-tooltips.html#tooltip-text-ggplotly
    ggplotly(tobler_plot)

    # should use geom_function instead
    # TODO: how to add shiny interactive input to geom_function()?
    # ggplot() + xlim(-60, 60) +
    #     geom_function(fun = toblers_hiking_function, n = 600)

    # TODO: Manually add second axis, as ggplotly() does not support dual axis
    # https://stackoverflow.com/questions/52833214/adding-second-y-axis-on-ggplotly
  })


  # Render walking time ----------
  # using lapply() would possibly be more efficient
  # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761


  # List of slopes used in examples
  slope_list <- c(0, 2.86, 20)

  lapply(1:3, function(example_num) {
    outputId <- glue::glue("eg{example_num}_uphill")
    # outputId <- paste0("eg", example_num, "_uphill")


    # All input$ elements needs to be inside a render() function
    output[[outputId]] <- renderText({
      slope_speed <- toblers_hiking_function(slope_list[example_num], input$speed) / 3.6
      time_render <- round(walking_time(slope_speed, input$custom_length), 2)

      glue::glue("Uphill: {time_render}s")
    })
  })

  
  lapply(1:3, function(example_num) {
    outputId <- glue::glue("eg{example_num}_downhill")
    # outputId <- paste0("eg", example_num, "_uphill")
    
    
    # All input$ elements needs to be inside a render() function
    output[[outputId]] <- renderText({
      slope_speed <- toblers_hiking_function(-slope_list[example_num], input$speed) / 3.6
      time_render <- round(walking_time(slope_speed, input$custom_length), 2)
      
      glue::glue("Downhill: {time_render}s")
    })
  })

  output$custom_uphill <- renderText({
    time_render <- round(walking_time(toblers_hiking_function(input$custom_slope, input$speed) / 3.6, input$custom_length), 2)
    glue::glue("Uphill: {time_render}s")
  })

  output$custom_downhill <- renderText({
    time_render <- round(walking_time(toblers_hiking_function(-input$custom_slope, input$speed) / 3.6, input$custom_length), 2)
    glue::glue("Downhill: {time_render}s")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
