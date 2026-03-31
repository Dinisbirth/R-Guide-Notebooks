library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(psych)
library(shiny)


data("penguins")

head(penguins)

description <- function(data){
  if (nrow(data) == 0) {
    return(data.frame(Mean = NA, Median = NA, SD = NA, Count = 0))
  }
  
  data.frame(
  Mean = mean(data$body_mass_g, na.rm = TRUE),
  Median = median(data$body_mass_g, na.rm = TRUE),
  SD = sd(data$body_mass_g, na.rm = TRUE),
  Count = nrow(data)
)
  
}

# SETUP Of UI

ui <- fluidPage(
  
  titlePanel("Penguins Dashboard"),
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("species_input",
                  "Select Species:",
                  choices = na.omit(unique(penguins$species))),
      
      selectInput("sex_input",
                  "Select Sex:",
                  choices = na.omit(unique(penguins$sex)))
    ),
    
    mainPanel(
      h3("Descriptive Summary"),
      tableOutput("summary_table"),
      
      h3("Histogram"),
      plotOutput("hist_plot")
    )
  )
  
)

server <- function(input, output){
  
  filtered_data <- reactive({
  penguins %>% 
    filter(species == input$species_input,
           sex == input$sex_input)
  })
  
  output$summary_table <- renderTable({
    validate(need(nrow(filtered_data()) > 0, "No data available for the selected filters."))
    description(filtered_data())
  })
  
  output$hist_plot <- renderPlot({
    validate(need(nrow(filtered_data()) > 0, "No data available for the selected filters."))
    ggplot(filtered_data(), aes(x = body_mass_g)) +
      geom_histogram(bins = 20)+
      labs(title = "Histogram of body mass")
  })
}

shinyApp(ui = ui, server = server)


