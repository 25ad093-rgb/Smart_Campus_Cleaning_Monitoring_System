library(shiny)

# Load data
cleaning_data <- read.csv("cleaning_data.csv")

# UI
ui <- fluidPage(
  
  titlePanel("Smart Campus Cleaning Monitoring System"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Report Cleaning Issue"),
      
      textInput(
        "location",
        "Location:",
        placeholder = "Example: Classroom 101"
      ),
      
      textInput(
        "issue",
        "Cleaning Issue:",
        placeholder = "Example: Dust / Garbage"
      ),
      
      selectInput(
        "priority",
        "Priority:",
        choices = c("High", "Medium", "Low")
      ),
      
      selectInput(
        "staff",
        "Assigned Staff:",
        choices = c(
          "Ravi",
          "Kumar",
          "Priya",
          "Anitha",
          "Suresh",
          "Meena"
        )
      ),
      
      actionButton(
        "submit",
        "Report Issue"
      )
    ),
    
    mainPanel(
      
      h3("Cleaning Records"),
      
      selectInput(
        "status",
        "Filter Status:",
        choices = c("All", "Pending", "Completed")
      ),
      
      tableOutput("cleaning_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Store current data
  data <- reactiveVal(cleaning_data)
  
  # Add new cleaning issue
  observeEvent(input$submit, {
    
    if (input$location == "" || input$issue == "") {
      
      showNotification(
        "Please enter location and issue.",
        type = "error"
      )
      
    } else {
      
      new_issue <- data.frame(
        ID = max(data()$ID) + 1,
        Location = input$location,
        Issue = input$issue,
        Priority = input$priority,
        Status = "Pending",
        Staff = input$staff,
        Date = as.character(Sys.Date())
      )
      
      updated_data <- rbind(
        data(),
        new_issue
      )
      
      data(updated_data)
      
      # Save to CSV
      write.csv(
        updated_data,
        "cleaning_data.csv",
        row.names = FALSE
      )
      
      showNotification(
        "Cleaning issue reported successfully!",
        type = "message"
      )
    }
  })
  
  # Filter records
  filtered_data <- reactive({
    
    result <- data()
    
    if (input$status != "All") {
      result <- result[
        result$Status == input$status,
      ]
    }
    
    result
  })
  
  # Display records
  output$cleaning_table <- renderTable({
    filtered_data()
  })
}

# Run application
shinyApp(
  ui = ui,
  server = server
)