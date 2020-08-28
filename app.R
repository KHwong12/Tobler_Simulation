
# Shiny application for simulating Tobler's hiking function

library(shiny)
library(shinythemes)

library(ggplot2)
library(plotly)

library(magrittr)
library(stringr)

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

  fluidRow(
    withMathJax(),


    column(
      5,

      strong(p("NOTE: This application is still in development.")),


      p("This web application lets you to play with the parameters of Tobler's hiking function to see how the walking speed varies with slope of the path.
             Tobler's hiking function is an exponential function determining the hiking speed, taking into account the slope angle.
             It was formulated by Waldo Tobler. This function was estimated from empirical data of Eduard Imhof."),

      withMathJax(p("The walking speed $W$ could be expressed as:")),

      # Render equation ------------------
      # double backslash required to avoid being truncated
      # Inline math symbols needs to wrap with withMathJax() function
      # https://shiny.rstudio.com/gallery/mathjax.html

      p("$$ W = Me^{-3.5 \\times \\left\\lvert tan \\theta + 0.05 \\right\\rvert} $$"),

      withMathJax(p("Where $\\theta$ is the slope and $M$ is maximum walking speed.")),

      br(),

      h3("How to Use"),

      # Require to add tags$ for list
      # https://shiny.rstudio.com/articles/tag-glossary.html

      tags$ol(
        tags$li("Drag the slider to change the walking speed on flat ground."),
        tags$li("Move along the plot on the right to observe the relationship."),
        tags$li("Check the walking time for the selected paths below.")
      ),

      br()
    ),

    column(
      7,
      sliderInput(
        inputId = "speed",
        label = "Walking speed on flat surface (km/hr)",
        min = 0.1, max = 8, step = .1,
        value = 5
      ),

      plotlyOutput("toblerPlot")
    )
  ),

  hr(),

  # Bottom panel ----------

  h3("How long does it take?"),

  p("With your walking speed given above, how long does take to finish the path with the slope of...?"),

  numericInput(
    inputId = "custom_length",
    label = "Path length (m)",
    min = 10, max = 1000, step = 10,
    value = 100,
    width = "20%"
  ),


  br(),

  p("The walking time on the following paths will be:"),

  fluidRow(
    column(
      3,
      h4("Flat Terrain"),
      img(src = "https://1.bp.blogspot.com/-wEygfnu5_mc/V5jHkBSzzLI/AAAAAAAA80w/DdLElofgt_Qn8RbZStkfWjnXhIH8n7cpgCLcB/s200/walking_businesswoman.png"),
      textOutput(outputId = "eg1_uphill"),
      textOutput(outputId = "eg1_downhill")
    ),

    column(
      3,
      h4("2.86° Slope"),
      img(src = "https://1.bp.blogspot.com/-59_nvImHVnM/XkZdUFSPVeI/AAAAAAABXWQ/Vbu2acjd6dwZjOoQIhRGeYjKPY2EtUCewCNcBGAsYHQ/s200/yagai_kyoushitsu_casual_walk.png"),
      textOutput(outputId = "eg2_uphill"),
      textOutput(outputId = "eg2_downhill")
    ),

    column(
      3,
      h4("20° Slope"),
      img(src = "https://2.bp.blogspot.com/-78mChg3NsLQ/VGLMgDJiciI/AAAAAAAApBk/3zAG9kQK1Fg/s200/noborizaka_saka.png"),
      textOutput(outputId = "eg3_uphill"),
      textOutput(outputId = "eg3_downhill")
    ),


    column(
      3,
      h4("What about..."),
      br(),
      sliderInput(
        inputId = "custom_slope",
        label = "Slope (degree)",
        min = 0, max = 45, step = .5,
        value = 5,
        width = "100%"
      ),

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

    dummydata <- data.frame(slope = seq(-45, 45, by = .05)) %>%
      mutate(speed = toblers_hiking_function(slope, input$speed))

    tobler_plot <- ggplot(dummydata, aes(x = slope, y = speed)) +
      # geom_line(color = "#2c3e50", aes(
      #   text = paste("Slope:",round(slope, 2), "degrees", "\nWalking Speed:", round(speed, 2), "km/hr")
      #   )) +
      geom_line(color = "#2c3e50") +
      geom_hline(yintercept = speed_flat_terrain, color = "#e7298a", linetype = "dotdash") +
      ylim(0, 10) +
      theme_minimal() +
      labs(
        x = "Slope of the path (°)",
        y = "Walking speed (km/hr)"
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
  # possibly use lapply would be more efficient
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
      
      glue::glue("Dowhill: {time_render}s")
    })
  })

  output$custom_uphill <- renderText({
    time_render <- round(walking_time(toblers_hiking_function(input$custom_slope, input$speed) / 3.6, input$custom_length), 2)
    glue::glue("Dowhill: {time_render}s")
  })

  output$custom_downhill <- renderText({
    time_render <- round(walking_time(toblers_hiking_function(-input$custom_slope, input$speed) / 3.6, input$custom_length), 2)
    glue::glue("Dowhill: {time_render}s")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
