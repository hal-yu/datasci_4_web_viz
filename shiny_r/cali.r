# Import necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Diabetes Age-adjusted Prevalence in California by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Choose a county:", choices = NULL)
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server logic
server <- function(input, output, session) {

# Load the dataset
  df <- reactive({
    url <- "https://raw.githubusercontent.com/hal-yu/datasci_4_web_viz/main/data/California.csv"
    read.csv(url)
  })

  # Filter the dataset
  df_diabetes <- reactive({
    data <- df()
    filter(data, MeasureId == "DIABETES", Data_Value_Type == "Age-adjusted prevalence")
  })
  
  # Update county choices dynamically based on dataset
  observe({
    diabetes_data <- df_diabetes()
    updateSelectInput(session, "county", choices = sort(unique(diabetes_data$LocationName)))
  })
  
  # Render the bar plot
  output$barPlot <- renderPlot({
    diabetes_data <- df_diabetes()
    county_data <- diabetes_data[diabetes_data$LocationName == input$county, ]
    avg_value <- mean(diabetes_data$Data_Value, na.rm = TRUE)
    
    ggplot() +
      geom_bar(data = county_data, aes(x = LocationName, y = Data_Value, fill = LocationName), stat = "identity") +
      geom_hline(aes(yintercept = avg_value), linetype = "dashed", color = "dodgerblue") +
      labs(title = 'Diabetes Age-adjusted Prevalence',
           y = 'Data Value (Age-adjusted prevalence) - Percent',
           x = 'Location (County)') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylim(0, 30) +
      scale_fill_manual(values = c("lightcoral", "dodgerblue"))
  })
  
}

# Run the Shiny app
shinyApp(ui, server)