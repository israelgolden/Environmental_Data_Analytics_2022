#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
# Read in PeterPaul processed dataset for nutrients. 
# Specify the date column as a date
# Remove negative values for depth_id 
# Include only lakename and sampledate through po4 columns
getwd()
nutrient_data <- read_csv("../Environmental_Data_Analytics_2022/Data/Processed/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <-  nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Nutrients in Peter Lake and Paul Lake"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select nutrient to plot
                    selectInput(inputId = "y", 
                                label = "Nutrient",
                                choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                                selected = "tp_ug"),
                    
                    # Select depth
                    checkboxGroupInput(inputId = "fill",
                                       label = "Depth ID",
                                       choices = unique(nutrient_data$depth_id),
                                       selected = c(1, 7)),
                    
                    # Select lake
                    checkboxGroupInput(inputId = "shape",
                                       label = "Lake",
                                       choices = c("Peter Lake", "Paul Lake"),
                                       selected = "Peter Lake"),
                    
                    # Select date range to be plotted
                    sliderInput(inputId = "x",
                                label = "Date",
                                min = as.Date("1991-05-01"),
                                max = as.Date("2016-12-31"),
                                value = c(as.Date("1995-01-01"), as.Date("1999-12-31")))),
                  
                  # Output
                  mainPanel(
                    plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
                    tableOutput("mytable")
                  )))

#### Define server  ----
server <- function(input, output) {
  
  # Define reactive formatting for filtering within columns
  filtered_nutrient_data <- reactive({
    nutrient_data %>%
      filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
      filter(depth_id %in% input$fill) %>%
      filter(lakename %in% input$shape) 
  })
  
  # Create a ggplot object for the type of plot you have defined in the UI  
  output$scatterplot <- renderPlot({
    ggplot(filtered_nutrient_data(), 
           aes_string(x = "sampledate", y = input$y, 
                      fill = "depth_id", shape = "lakename")) +
      geom_point(alpha = 0.8, size = 2) +
      theme_classic(base_size = 14) +
      scale_shape_manual(values = c(21, 24)) +
      labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
      scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
    #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
  })
  
  # Create a table that generates data for each point selected on the graph  
  output$mytable <- renderTable({
    brush_out <- brushedPoints(filtered_nutrient_data(), input$scatterplot_brush)
  })
  
}

shinyApp(ui = ui, server = server)
