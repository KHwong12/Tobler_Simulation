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
                        "Walking Speed",
                        min = 0.1,
                        max = 10,
                        value = 6)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {

    # output$distPlot <- renderPlot({
    # 
    # 
    #     plot <- ggplot(data.frame(x = c(-60, 60)), aes(x)) +
    #         stat_function(fun = function(x) toblers_hiking_function(slope_angle = x, max_speed = input$speed), n = 600) +
    #         ylim(0,6) +
    #         theme_minimal()
    #     
    #     plot
    # 
    # })
    
    
    

    
    
    output$distPlot <- renderPlotly({

        req(input$speed)
        
        
        
        dummydata <- data.frame(slope = seq(-60, 60, by = .2)) %>%
            mutate(speed = toblers_hiking_function(slope_angle = slope, max_speed = input$speed))
        

        
        # tobler_plot <- ggplot(dummydata, aes(x = slope, y = speed)) +
        #     geom_line()
        # 
        # ggplotly(tobler_plot)
        
        plot_ly(dummydata, x = ~slope, y = ~speed) %>% add_lines()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
