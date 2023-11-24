# Install shinyjs and DT packages if not installed
# install.packages(c("shinyjs", "DT", "shinyalert", "readxl", "openxlsx"))

# Load required libraries
library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(shinyalert)
library(openxlsx)

# Define UI
ui <- fluidPage(
  titlePanel("Excel File Reader"),
  useShinyjs(),  # Initialize shinyjs
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File", accept = c(".xlsx", ".xls")),
      tags$hr(),
      actionButton("selectColumnsBtn", "Select Columns"),
      tags$hr(),
      tags$hr(),
      actionButton("allocateCategoryBtn", "Allocate Category"),
      tags$hr(),
      selectInput("categoryDropdown", "Select Category", choices = c("Rent", "Travel", "Groceries")),
      tags$hr(),
      actionButton("saveChangesBtn", "Save Changes")
    ),
    mainPanel(
      DTOutput("datatable"),
      DTOutput("modifiedDatatable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive functions to read the uploaded file and store data
  data <- reactiveVal(NULL)
  modified_data <- reactiveVal(NULL)
  
  observe({
    req(input$file) # Require that a file has been uploaded
    inFile <- input$file
    
    # Check if the file is an Excel file
    if (grepl("\\.xls$|\\.xlsx$", inFile$name, ignore.case = TRUE)) {
      # Read the Excel file using read_excel
      data_val <- readxl::read_excel(inFile$datapath)
      
      # Add a new column "exp_cat" to the data with default values
      data_val$exp_cat <- rep("Category1", nrow(data_val))  # Default value, you can modify this based on user input
      
      # Update the reactiveVal with the new data
      data(data_val)
    } else {
      # If the file is not an Excel file, return NULL
      data(NULL)
    }
  })
  
  # Show modal dialog to select columns
  observeEvent(input$selectColumnsBtn, {
    showModal(modalDialog(
      selectInput("amount_col", "Select Amount Column", ""),
      selectInput("type_col", "Select Expense Type Column", ""),
      selectInput("third_col", "Select Third Column", ""),
      footer = tagList(
        actionButton("confirmColumnsBtn", "Confirm"),
        modalButton("Cancel")
      )
    ))
    
    # Update choices for amount_col in modal dialog
    observe({
      updateSelectInput(session, "amount_col", choices = names(data()))
    })
    
    # Update choices for type_col in modal dialog
    observe({
      updateSelectInput(session, "type_col", choices = names(data()))
    })
    
    # Update choices for third_col in modal dialog
    observe({
      updateSelectInput(session, "third_col", choices = names(data()))
    })
  })
  
  # Confirm button event
  observeEvent(input$confirmColumnsBtn, {
    # Hide the modal dialog
    removeModal()
    
    # Select columns based on user input
    selected_data <- data()[, c(input$amount_col, input$type_col, input$third_col, "exp_cat"), drop = FALSE]
    
    # Render an interactive table with editable dropdown for "exp_cat" column
    output$datatable <- renderDT({
      datatable(
        selected_data,
        selection = 'multiple',  # Allow multiple row selection
        editable = list(target = 'cell', disable = list(columns = c(0, 1, 2)))
      )
    })
  })
  
  # Allocate Category button event
  observeEvent(input$allocateCategoryBtn, {
    # Check if rows are selected
    if (!is.null(input$datatable_rows_selected)) {
      selected_rows <- input$datatable_rows_selected
      category <- input$categoryDropdown
      modified_data_val <- data()[selected_rows, , drop = FALSE]
      modified_data_val$exp_cat <- category
      modified_data(modified_data_val)
    } else {
      # Show an alert if no rows are selected
      shinyalert("Alert", "Please select one or more rows.", type = "error")
    }
  })
  
  # Save Changes button event
  observeEvent(input$saveChangesBtn, {
    # Check if changes have been made
    if (!is.null(modified_data()) && !is.null(data())) {
      # Save modified data to a new Excel file
      write.xlsx(modified_data(), "path/to/your/new_file.xlsx", row.names = FALSE)
      print("Changes saved!")
    } else {
      shinyalert("Alert", "No changes to save.", type = "info")
    }
  })
  
  # Render modified DataTable
  output$modifiedDatatable <- renderDT({
    if (!is.null(modified_data())) {
      datatable(
        modified_data(),
        selection = 'none',  # Disable row selection for the modified DataTable
        editable = FALSE  # Disable editing for the modified DataTable
      )
    }
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
