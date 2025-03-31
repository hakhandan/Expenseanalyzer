#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load the required libraries
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tcltk)
library(plotly)
library(ggplotify)

# Define the check_for_keywords function
check_for_keywords <- function(data) {
  keywords <- c("Transfer", "Me Bnz", "Clearing House Ltd", "Western Unio", "Gm Imt", "Emirates", "Rav4", "Georgian National Un")
  has_keywords <- apply(data, MARGIN = 1, function(row) any(grepl(paste(keywords, collapse = "|"), row, ignore.case = TRUE)))
  return(data[!has_keywords, ])
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      /* Style the buttons */
      .btn-primary {
        background-color: #4CAF50; /* Green */
        border: none;
        color: white;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 10px;
      }

      /* Style the 'Clear Data' button */
      #clearDataButton {
        background-color: #f44336; /* Red */
      }

      /* Style the panel background */
      .sidebarPanel {
        background-color: #e7e7e7; /* Light gray */
        padding: 20px;
        border-radius: 15px;
      }
      "
    ))
  ),
  titlePanel("Expense Analyzer"),
  sidebarLayout(
    sidebarPanel(
      h3("Upload Excel Files"),
      fileInput("file1", "Choose Excel File 1"),
      fileInput("file2", "Choose Excel File 2"),
      downloadButton("downloadData", "Download Filtered Data", class = "btn-primary"),
      downloadButton("downloadSummary", "Download Summary Data", class = "btn-primary"),
      #downloadButton("downloadBarChart", "Download Bar Chart", class = "btn-primary"),
      actionButton("clearDataButton", "Clear Data", class = "btn-primary"),
      p("Notice: You olny need to download transaction files of your ANZ Check and Saving accounts (any time range desired). Thats it!!"),
      p("Credit: HANK")
      
    ),
    mainPanel(
      h3("Summary Data"),
      tableOutput("summaryData"),
      plotlyOutput("barChart")
    )
  )
)

server <- function(input, output) {
  data <- reactiveVal(NULL)
  observeEvent(input$clearDataButton, {
    data(NULL)
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    data1 <- read_xlsx(input$file1$datapath)
    data(data1)
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    data2 <- read_xlsx(input$file2$datapath)
    if (!is.null(data())) {
      combined_data <- bind_rows(data(), data2)
      data(check_for_keywords(combined_data))
    } else {
      data(data2)
    }
  })
  
  # Define the list of keywords for each category
  category_keywords <- list(
    Groceries = c("The Fruit Sh","3798","Shopping", "Moshims", "Newtown Green Grocer", "Harbourside Market", "Pak N Save P", "Pak N Save K", "Morteza Bake", "Countdown Ka", "New World Th", "Wellington Halal Mea", "Moore Wilsons", "New World Oh", "Halal Meat S"),
    Eat_out = c("Wellington S", "Cafe Vue - M", "Bubee Drinks", "Mcdonalds Pe", "Corfu Seafoods", "Hell Pizza N", "Mojo Tahi", "Lambton Squa", "Lunchonline", "Subway Mulgr", "Mcdonalds Ta", "Sakura Sushi", "Pizza Hut", "Mcdonalds Ne", "The Warehous", "Subway Karor", "Pizza Hut Ka", "Play Queensg", "Karori Park", "Mcdonalds Lo", "Umi Sushi", "Nando'S Lowe", "Noodle Plus Woodward", "Cafe On The", "Coffix Moles", "Lunch Box"),
    Clothing_etc = c("St Pierre'S", "Mcdonalds La", "Paperplusselect Karo", "Mitre 10 Cro", "Farmers", "Kmart - Peto", "Postie Queen", "Rebel Wellin", "Delaware Nor", "H&M", "Briscoes Wel", "Farmers Queen"),
    Commute = c("Snapper Serv", "Bp 2Go Newla", "Wcc Parking", "Aa Insurance Pre", "Museum Of Nz", "Care Park We", "Museum Of Ne", "Nz Transport", "Bp 2Go Karor", "New World Fuel Levin", "Bp Connect T", "Aksal Motors Limited"),
    Entertainment = c("Spotify P24E", "Dellmont.Com", "Fifa", "Apple.Com/Bi", "Netflix"),
    Health = c("Unichem Mole", "Karori Medic", "Unichem Well", "Unichem Karo", "Southern Cross Healt"),
    Internet_power = c("2Degrees Mob", "Mercury Nz Ltd", "2Degrees", "Two Degrees Nz Ltd"),
    Childcare = c("Kindercare Learning"),
    Loan = c("Gem Visa"),
    Arta_Piano = c("Raewyn Brockway"),
    Rent = c("Dwelling Property")
  )
  
  filtered_data <- reactive({
    data_val <- data()
    if (is.null(data_val)) {
      return(NULL)
    }
    filtered_data <- data_val %>%
      select("Processed Date", "Type", "Code", "Reference", "Amount", "Details", "Particulars")
    
    assign_category <- function(text) {
      for (category in names(category_keywords)) {
        keywords <- category_keywords[[category]]
        for (keyword in keywords) {
          if (grepl(paste0("^", substr(keyword, 1, 6)), substr(text, 1, 6), ignore.case = TRUE)) {
            return(category)
          }
        }
      }
      return("others")
    }
    
    filtered_data$Exp_type <- mapply(assign_category, filtered_data$Code)
    filtered_data$Exp_type[filtered_data$Exp_type == "others"] <- mapply(assign_category, filtered_data$Details[filtered_data$Exp_type == "others"])
    filtered_data$Exp_type[filtered_data$Exp_type == "others"] <- mapply(assign_category, filtered_data$Particulars[filtered_data$Exp_type == "others"])
    
    filtered_data <- filtered_data %>%
      filter(Amount <= 0)
    
    filtered_data
  })
  
  summary_data <- reactive({
    data_val <- filtered_data()
    if (is.null(data_val)) {
      return(NULL)
    }
    summary_data <- data_val %>%
      group_by(Exp_type) %>%
      summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
      ungroup()
    
    total_amount <- sum(summary_data$Total_Amount, na.rm = TRUE)
    total_data <- tibble(Exp_type = "Total", Total_Amount = total_amount)
    summary_data <- bind_rows(summary_data, total_data)
    
    summary_data
  })
  
  output$summaryData <- renderTable({
    data_val <- summary_data()
    if (is.null(data_val)) {
      return(NULL)
    }
    data_val
  })
  
  output$barChart <- renderPlotly({
    bar_chart_data <- summary_data()
    if (is.null(bar_chart_data)) {
      return(NULL)
    }
    bar_chart_data <- bar_chart_data %>%
      filter(Exp_type != "Total")
    
    bar_chart_data$Exp_type <- factor(bar_chart_data$Exp_type, levels = bar_chart_data$Exp_type)
    
    p <- ggplot(bar_chart_data, aes(x = Exp_type, y = Total_Amount, fill = Exp_type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Total_Amount), vjust = -0.5, size = 2) +
      labs(title = "Expense Distribution by Category") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data", ".csv", sep = "")
    },
    content = function(file) {
      data_val <- filtered_data()
      if (!is.null(data_val)) {
        write.csv(data_val, file, row.names = FALSE)
      }
    }
  )
  
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("summary_data", ".csv", sep = "")
    },
    content = function(file) {
      data_val <- summary_data()
      if (!is.null(data_val)) {
        write.csv(data_val, file, row.names = FALSE)
      }
    }
  )
  
  output$downloadBarChart <- downloadHandler(
    filename = function() {
      paste("bar_chart", ".png", sep = "")
    },
    content = function(file) {
      bar_chart_data <- summary_data()
      if (!is.null(bar_chart_data)) {
        ggplotly(ggsave(file, output$barChart))
      }
    }
  )
}

shinyApp(ui, server)