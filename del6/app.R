# Install shinyjs and DT packages if not installed
# install.packages(c("shinyjs", "DT", "shinyalert", "readxl"))

# Load required libraries
library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(shinyalert)

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
      actionButton("addCategoryBtn", "Add Category"),
      tags$hr()
    ),
    mainPanel(
      DTOutput("datatable"),
      DTOutput("newDatasetTable")  # Add a new DTOutput for the new dataset table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to read the uploaded file
  data <- reactiveVal(NULL)

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
  })
  
  # Render the interactive table with the selected columns
  output$datatable <- renderDT({
    # Check if all three columns are selected after uploading the file
    if (!is.null(data()) && 
        !is.null(input$amount_col) && 
        !is.null(input$type_col) && 
        !is.null(input$third_col)) {
      
      # Select columns based on user input
      selected_data <- data()[, c(input$amount_col, input$type_col, input$third_col, "exp_cat"), drop = FALSE]
      
      # Render an interactive table with editable dropdown for "exp_cat" column
      datatable(
        selected_data,
        selection = 'single',
        editable = list(target = 'cell', disable = list(columns = c(0, 1, 2)))
      )
    }
  })
  
  # Add Category button event
  observeEvent(input$addCategoryBtn, {
    # Check if a row is selected
    if (!is.null(input$datatable_cell_edit)) {
      info = input$datatable_cell_edit
      str(info)
      modified_data <- data()
      modified_data[info$row, "exp_cat"] <- info$value
      data(modified_data)
    }
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
