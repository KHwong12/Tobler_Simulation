#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("speed",
                        "Walking speed on flat surface (km/hr)",
                        min = 0.1,
                        max = 8,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("toblerPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {
  
    output$toblerPlot <- renderPlotly({

        req(input$speed)
        
        speed_flat_terrain = toblers_hiking_function(0, flat_terrain_speed = input$speed)
        
        dummydata <- data.frame(slope = seq(-60, 60, by = .2)) %>%
            mutate(speed = toblers_hiking_function(slope_angle = slope, flat_terrain_speed = input$speed))

        tobler_plot <- ggplot(dummydata, aes(x = slope, y = speed)) +
            geom_line() +
            geom_hline(yintercept = speed_flat_terrain, color = "red") +
            ylim(0,10)
        
        
        # should use geom_function instead
        # TODO: how to add shiny interactive input to geom_function()?
        # ggplot() + xlim(-60, 60) +
        #     geom_function(fun = toblers_hiking_function, n = 600)
        

        ggplotly(tobler_plot)

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
