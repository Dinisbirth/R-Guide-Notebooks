library(dplyr)
library(ggplot2)
library(shiny)

get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  current_frame <- sys.frames()[[1]]
  if (!is.null(current_frame$ofile)) {
    return(dirname(normalizePath(current_frame$ofile)))
  }

  normalizePath(getwd())
}

script_dir <- get_script_dir()
project_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
input_file <- file.path(project_dir, "data", "processed", "Jobs_clean.csv")

if (!file.exists(input_file)) {
  stop("Required processed dataset was not found at week11-project/data/processed/Jobs_clean.csv. Run scripts/week11_project.R first.")
}

JobsClean <- read.csv(input_file, stringsAsFactors = FALSE)

if (!"python_required" %in% names(JobsClean)) {
  JobsClean$python_required <- ifelse(JobsClean$python == 1, "Yes", "No")
}

if (!"rating_clean" %in% names(JobsClean)) {
  JobsClean$rating_clean <- ifelse(JobsClean$rating == 0, NA, JobsClean$rating)
}

JobsClean$job_simp[is.na(JobsClean$job_simp) | JobsClean$job_simp == ""] <- "Unknown"
JobsClean$sector[is.na(JobsClean$sector) | JobsClean$sector == ""] <- "Unknown"
JobsClean$job_state[is.na(JobsClean$job_state) | JobsClean$job_state == ""] <- "Unknown"

summary_table <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame(Metric = "Records", Value = 0))
  }

  data.frame(
    Metric = c("Records", "Mean Salary", "Median Salary", "Mean Rating", "Python Required Share"),
    Value = c(
      nrow(data),
      round(mean(data$avg_salary, na.rm = TRUE), 2),
      round(median(data$avg_salary, na.rm = TRUE), 2),
      round(mean(data$rating_clean, na.rm = TRUE), 2),
      round(mean(data$python_required == "Yes", na.rm = TRUE), 2)
    )
  )
}

salary_min <- floor(min(JobsClean$avg_salary, na.rm = TRUE))
salary_max <- ceiling(max(JobsClean$avg_salary, na.rm = TRUE))

ui <- fluidPage(
  titlePanel("Week 11 Jobs Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "job_simp_input",
        "Job Type",
        choices = c("All", sort(unique(JobsClean$job_simp))),
        selected = "All"
      ),
      selectInput(
        "sector_input",
        "Sector",
        choices = c("All", sort(unique(JobsClean$sector))),
        selected = "All"
      ),
      radioButtons(
        "python_input",
        "Python Required",
        choices = c("All", "Yes", "No"),
        selected = "All",
        inline = TRUE
      ),
      sliderInput(
        "salary_range",
        "Average Salary Range",
        min = salary_min,
        max = salary_max,
        value = c(salary_min, salary_max)
      )
    ),
    mainPanel(
      h3("Filtered Summary"),
      tableOutput("summary_output"),
      h3("Salary Distribution"),
      plotOutput("hist_plot"),
      h3("Top Job States"),
      plotOutput("state_plot"),
      h3("Rating vs Salary"),
      plotOutput("scatter_plot"),
      h3("Salary by Job Type"),
      plotOutput("box_plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- JobsClean %>%
      filter(
        avg_salary >= input$salary_range[1],
        avg_salary <= input$salary_range[2]
      )

    if (input$job_simp_input != "All") {
      data <- data %>% filter(job_simp == input$job_simp_input)
    }

    if (input$sector_input != "All") {
      data <- data %>% filter(sector == input$sector_input)
    }

    if (input$python_input != "All") {
      data <- data %>% filter(python_required == input$python_input)
    }

    data
  })

  output$summary_output <- renderTable({
    summary_table(filtered_data())
  })

  output$hist_plot <- renderPlot({
    validate(need(nrow(filtered_data()) > 0, "No data available for the selected filters."))

    ggplot(filtered_data(), aes(x = avg_salary)) +
      geom_histogram(binwidth = 10, fill = "steelblue", colour = "black") +
      labs(
        title = "Average Salary Distribution",
        x = "Average salary",
        y = "Count"
      ) +
      theme_minimal()
  })

  output$state_plot <- renderPlot({
    validate(need(nrow(filtered_data()) > 0, "No data available for the selected filters."))

    state_counts <- filtered_data() %>%
      count(job_state, sort = TRUE) %>%
      slice_head(n = 10)

    ggplot(state_counts, aes(x = reorder(job_state, n), y = n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(
        title = "Top Job States",
        x = "State",
        y = "Count"
      ) +
      theme_minimal()
  })

  output$scatter_plot <- renderPlot({
    scatter_data <- filtered_data() %>%
      filter(!is.na(rating_clean))

    validate(need(nrow(scatter_data) > 0, "No rating data available for the selected filters."))

    ggplot(scatter_data, aes(x = rating_clean, y = avg_salary, colour = python_required)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Rating vs Average Salary",
        x = "Company rating",
        y = "Average salary",
        colour = "Python required"
      ) +
      theme_minimal()
  })

  output$box_plot <- renderPlot({
    validate(need(nrow(filtered_data()) > 0, "No data available for the selected filters."))

    box_data <- filtered_data() %>%
      mutate(plot_group = if_else(dplyr::n_distinct(job_simp) == 1, "Selected Jobs", job_simp))

    ggplot(box_data, aes(x = plot_group, y = avg_salary, fill = plot_group)) +
      geom_boxplot(show.legend = FALSE) +
      labs(
        title = "Average Salary by Job Type",
        x = "Job type",
        y = "Average salary"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
