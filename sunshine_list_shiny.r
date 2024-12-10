library(readxl)
library(dplyr)  
library(ggplot2)
library(shiny)
library(DT)
library(stringr)


salaries <- read.csv("/Users/clayelzinga/Downloads/tbs-pssd-compendium-salary-disclosed-2023-en-utf-8-2024-05-31.csv")
dim(salaries)
head(salaries)


ui <- fluidPage(
  titlePanel("Salary Dataset Multi-Filter Search"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("last_name", "Filter by Last Name:", placeholder = "Enter last name..."),
      textInput("first_name", "Filter by First Name:", placeholder = "Enter first name..."),
      textInput("employer", "Filter by Employer:", placeholder = "Enter employer..."),
      textInput("job_title", "Filter by Job Title:", placeholder = "Enter job title..."),
      numericInput("min_salary", "Minimum Salary:", value = NA, min = 0),
      numericInput("max_salary", "Maximum Salary:", value = NA, min = 0),
      numericInput("year", "Filter by Year:", value = NA, min = 2000, max = 2100),
      actionButton("submit", "Apply Filters"),
      width = 4
    ),
    
    mainPanel(
      DTOutput("results_table")
    )
  )
)


server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$submit) 
    data <- salaries
    if (input$last_name != "") {
      data <- data %>% filter(grepl(input$last_name, Last.Name, ignore.case = TRUE))
    }
    if (input$first_name != "") {
      data <- data %>% filter(grepl(input$first_name, First.Name, ignore.case = TRUE))
    }
    if (input$employer != "") {
      data <- data %>% filter(grepl(input$employer, Employer, ignore.case = TRUE))
    }
    if (input$job_title != "") {
      data <- data %>% filter(grepl(input$job_title, JobTitle, ignore.case = TRUE))
    }
    if (!is.na(input$min_salary)) {
      data <- data %>% filter(Salary >= input$min_salary)
    }
    if (!is.na(input$max_salary)) {
      data <- data %>% filter(Salary <= input$max_salary)
    }
    if (!is.na(input$year)) {
      data <- data %>% filter(Year == input$year)
    }
    
    data
  })
  output$results_table <- renderDT({
    input$submit
    isolate({
      filtered_data() %>%
        datatable(options = list(pageLength = 10))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)









