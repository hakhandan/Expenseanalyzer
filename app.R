# Load the required libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tcltk)
library(plotly)
library(ggplotify)
library(colourpicker)
library("shinyjs")
library(viridis)
library(DT)
library(shinyalert)

# Define the check_for_keywords function
check_for_keywords <- function(data) {
  keywords <- c("Transfer", "Gm Imt", "Rav4", "Etoro", "Binance", "Me Bnz", "Saving t", "Online Payment")
  has_keywords <- apply(data, MARGIN = 1, function(row) any(grepl(paste(keywords, collapse = "|"), row, ignore.case = TRUE)))
  return(data[!has_keywords, ])
}

ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(title = "ANZ Expense Analyzer"),
  sidebar = dashboardSidebar(
    tags$style(HTML(
      "
    /* Style the sidebar background */
    .sidebar {
      background-color: black; /* Change this to your desired color */
      padding: 20px;
      border-radius: 15px;
    }

    /* Style the otherUnidentifiedTable background */
    #otherUnidentifiedTable {
      background-color: lightblue; /* Keep this as is or change if needed */
    }
    "
    )),
    sidebarMenu(
      menuItem("Upload Excel Files", tabName = "upload", icon = icon("file-excel")),
      menuItem("Download Filtered Data", tabName = "downloadData", icon = icon("download")),
      menuItem("Download Summary Data", tabName = "downloadSummary", icon = icon("download")),
      p(
        style = "white-space: pre-line;",
        HTML("<strong>Notice:</strong>"),
        "You can upload up to three files. You need to download transaction files of your ANZ accounts (Check, Saving and Credit card) in Excel format, any time range desired). That's it!!"
      ),
      p(
        style = "white-space: pre-line;",
        "The order of files is not important. Also, if you have two checking accounts, you can use any files to upload the two (for example, file 1 and 2 can be two checking acc transactions.)"
      ),
      p(
        style = "white-space: pre-line;",
        HTML("<strong>By: Hank</strong>")
      )
    )
  ),  # Add a comma here
  body = dashboardBody(
    useShinyjs(),  # Enable shinyjs
    tabItems(
      tabItem(
        tabName = "upload",
        fluidRow(
          tabBox(
            width = 12,
            title = "Upload Files",
            id = "upload-tab",
            tabPanel(
              "Upload",
              fileInput("file1", "Choose Excel File 1 "),
              fileInput("file2", "Choose Excel File 2 "),
              fileInput("file3", "Choose Excel File 3 "),
              actionButton("clearDataButton", "Clear Data", class = "btn-primary", style = "background-color: #28a745; border-color: #28a745;")
            )
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            title = "Data Summary",
            id = "summary-tab",
            tabPanel(
              "Summary",
              HTML('<h3 id="summaryDataText" style="display: none;">Summary Data</h3>'),
              DTOutput("summaryDataTable"),
              plotlyOutput("barChart"),
              #plotlyOutput("pieChart"),  # Change this line
              
              tableOutput("otherUnidentifiedTable")
            )
          )
        )
      ),
      tabItem(
        tabName = "downloadData",
        fluidPage(
          downloadButton("downloadData", "Download Filtered Data", class = "btn-primary")
        )
      ),
      tabItem(
        tabName = "downloadSummary",
        fluidPage(
          downloadButton("downloadSummary", "Download Summary Data", class = "btn-primary")
        )
      )
    )
  )
)



server <- function(input, output) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$clearDataButton, {
    data(NULL)
  })
  
  observe({
    # Hide or show the summary data text based on data availability
    if (!is.null(filtered_data())) {
      shinyjs::runjs('$("#summaryDataText").show();')
    } else {
      shinyjs::runjs('$("#summaryDataText").hide();')
    }
  })
  
  observe({
    data_val <- summary_data()
    if (!is.null(data_val)) {
      # Check for positive or negative "Total Save" and show popup accordingly
      total_save <- data_val$Total.Amount[data_val$Expense.Category == "Total Save"]
      if (length(total_save) > 0) {
        if (total_save > 0) {
          shinyalert(title = "Mate!",
                     text = "It seems you have saved some money! Go party!",
                     closeOnClickOutside = TRUE,
                     animation = "pop",
                     timer = 2000,
                     size = "xs",
                     type = "success")
        } else {
          shinyalert(title = "Ooh mate!",
                     text = "Negative Total Save? Your savings must be on vacation... without you!
                     
                     Or you may just need to upload more files?",
                     closeOnClickOutside = TRUE,
                     animation = "pop",
                     timer = 4000,
                     size = "xs",
                     type = "error")
        }
      }
    }
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    data1 <- read_xlsx(input$file1$datapath)
    data(check_for_keywords(data1))
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    data2 <- read_xlsx(input$file2$datapath)
    if (!is.null(data())) {
      combined_data <- bind_rows(data(), data2)
      data(check_for_keywords(combined_data))
    } else {
      data(check_for_keywords(data2))
    }
  })
  
  observeEvent(input$file3, {
    req(input$file3)
    data3 <- read_xlsx(input$file3$datapath)
    if (!is.null(data())) {
      combined_data <- bind_rows(data(), data3)
      data(check_for_keywords(combined_data))
    } else {
      data(check_for_keywords(data3))
    }
  })
  
  # Define the list of keywords for each category
  category_keywords <- list(
    Groceries = c("The Fruit Sh","Four Square	","Hanna","Nastaran ","Mediterranea","Super Liquor", "Shopping","Shopping", "Nespresso Nz", "Moshims", "Newtown Green Grocer", "Harbourside Market", "Pak N Save P", "Pak N Save K", "Morteza Bake", "Countdown Ka", "New World Th", "Wellington Halal Mea", "Moore Wilsons", "New World Oh", "Halal Meat S"),
    
    Eat_out = c("Wellington S","Burger","Old Baily","Basin Noodle","Number One","Home","Burger King","Bethel Woods", "Flying Burri", "Kilim", "Aroy Thai Th", "The Old Bail", "Brumbys", "Cafe Vue - M", "Bubee Drinks", "Mcdonalds Pe", "Corfu Seafoods", "Hell Pizza N", "Mojo Tahi", "Lunchonline", "Subway Mulgr", "Mcdonalds Ta", "Sakura Sushi", "Pizza Hut", "Mcdonalds Ne", "The Warehous", "Subway Karor", "Pizza Hut Ka", "Play Queensg", "Karori Park", "Mcdonalds Lo", "Umi Sushi", "Nando'S Lowe", "Noodle Plus Woodward", "Cafe On The", "Coffix Moles", "Lunch Box", "Domino"),
    
    General_shoppings = c("Mary Potter Hospice","Desk","Vending Dire","Coin City","Lambton Quay","Cuba Mall","ATM Debit","Inland Revenue","Cw Glenfield","Dept Interna","Iranian Embassy","Tsb Living	","Trademe ","Patpat","Cotton On","Payback","Harvey Norma", "Baby ","Noel L" ,"Tommy Hilfig", "St Pierre'S", "Ezibuy", "Aliexpress", "Temu", "Salvation", "Mcdonalds La", "Paperplusselect Karo", "Mitre 10 Cro", "Farmers", "Kmart - Peto", "Postie Queen", "Rebel Wellin", "Delaware Nor", "H&M", "Briscoes Wel", "Farmers Queen"),
    
    Travel = c("Snapper Serv","Repco","Allied Petroleum","Aphg Nz Inve","Uber Trip","Kaitoke","Viaduct ","Nzaa Online","Dubai Intern", "Ayda Mortazavi","Brendan ", "Camping","Clearing House Ltd","Clearing House","Mansour Zamanpor", "emirates", "Air New Zealand", "Parkmate Nz","Jetstar", "Bp 2Go Newla", "Airbnb", "Ohakune Trip", "Wcc Parking", "Caltex ", "Parking", "Aa Insurance Pre", "Museum Of Nz", "Mobil", "Care Park We", "Museum Of Ne", "Nz Transport", "Bp 2Go Karor", "New World Fuel Levin", "Bp Connect T", "Aksal Motors Limited","Gull","Challenge Fue","NPD","Waitomo","Z Energy"),
    
    Entertainment = c("Spotify P24E","Freemans Lot", "Tip Busker O","Hangdog Adve", "Dellmont.Com", "Fifa", "Apple.Com/Bi", "Netflix", "Amazon", "Junglerama"),
    
    Health_Beauty = c("Unichem Mole","Afterhours ","Southern/acc","Laser Clinic", "Physique Stu","Karori Medic", "Unichem Well", "Unichem Karo", "Southern Cross Healt", "Bowen Radiol","Just Cuts", "Lcnz Courten","Mecca Wellin	","Lotte Travel","Shavers Quee","Beyond Skin"),
    
    Internet_Power = c("2Degrees Mob", "Mercury Nz Ltd", "2Degrees", "Two Degrees Nz Ltd", "Contact", "Flick", "Electric Kiwi", "Genesis", "Vodafone", "Powershop", "Frank energy", "nova", "Nova", "Slingshot", "slingshot","Ecotricity", "Transpower", "Grey Power", "Reliance", "Superpower", "Meridian", "genesis", "Pulse", "pulse", "Globug", "Raw Energy", "Spark NZ", "One", "Zeronet", "Compass", "Now", "Now.", "Hotshot", "Skinny", "voyager", "Fibercity", "Orcon","Trustpower","Trust power"),
    
    Childcare = c("Kindercare Learning", "Baby sit","Sling", "Kindy", "Kindergarten", "Kathlyn", "Nothland", "Whanau"),
   
     Loan = c("Gem Visa", "Mortgage"),
    
    Children_edu = c("Raewyn Brockway", "Piano", "KWNS","Scholastic"),
   
     Rent = c("Dwelling Property", "Rent")
  )
  
  filtered_data <- reactive({
    data_val <- data()
    if (is.null(data_val)) {
      return(NULL)
    }
    filtered_data <- data_val %>%
      select("Processed Date", "Type", "Code", "Reference", "Amount", "Details", "Particulars")
    
    assign_category <- function(text) {
      # Check for "Cafe" (case-insensitive) in the text
      if (grepl("cafe", text, ignore.case = TRUE)) {
        return("Eat_out")
        
      }
      
      
      for (category in names(category_keywords)) {
        keywords <- category_keywords[[category]]
        for (keyword in keywords) {
          if (grepl(paste0("^", substr(keyword, 1, 5)), substr(text, 1, 5), ignore.case = TRUE)) {
            return(category)
          }
        }
      }
      return("Unidentified")
    }
    
    filtered_data$`Expense.Category` <- mapply(assign_category, filtered_data$Code)
    filtered_data$`Expense.Category`[filtered_data$`Expense.Category` == "Unidentified"] <- mapply(assign_category, filtered_data$Details[filtered_data$`Expense.Category` == "Unidentified"])
    filtered_data$`Expense.Category`[filtered_data$`Expense.Category` == "Unidentified"] <- mapply(assign_category, filtered_data$Particulars[filtered_data$`Expense.Category` == "Unidentified"])
    
    
    # Consider positive values as income
    filtered_data$Expense.Category[filtered_data$Amount > 0] <- "Income"
    # filtered_data$Amount[filtered_data$Amount > 0] <- 0
    
    filtered_data
  })
  
  # Summary table data
  summary_data <- reactive({
    data_val <- filtered_data()
    if (is.null(data_val)) {
      return(NULL)
    }
    
    summary_data <- data_val %>%
      group_by(Expense.Category) %>%
      summarize(Total.Amount = sum(Amount, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Total.Amount)
    
    # Exclude "Income" category from the total sum
    total_income <- sum(data_val$Amount[data_val$Expense.Category == "Income"], na.rm = TRUE)
    total_data_income <- tibble(Expense.Category = "Total Income", Total.Amount = total_income)
    
    # Exclude "Income" category from the summary table
    summary_data <- summary_data[summary_data$Expense.Category != "Income", ]
    
    # Calculate and add "Total_Expenses" row
    total_expenses <- sum(data_val$Amount[data_val$Amount < 0], na.rm = TRUE)
    total_data_expenses <- tibble(Expense.Category = "Total Expenses", Total.Amount = total_expenses)
    
    # Calculate and add "Total_Save" row
    total_save <- total_income + total_expenses
    total_data_save <- tibble(Expense.Category = "Total Save", Total.Amount = total_save)
    
    # Add the "Total Income", "Total Expenses", "Total Save" rows
    summary_data <- bind_rows(summary_data,  total_data_expenses,total_data_income, total_data_save)
    
    summary_data
  })
  
  
  output$summaryDataTable <- renderDT({
    data_val <- summary_data()
    if (!is.null(data_val)) {
      data_val$`Expense.Category` <- gsub("Health_Beauty", "Health/Beauty", data_val$`Expense.Category`)
      data_val$`Expense.Category` <- gsub("Internet_Power", "Internet/Power", data_val$`Expense.Category`)
      data_val$`Expense.Category` <- gsub("General_shoppings", "General shoppings", data_val$`Expense.Category`)
      
      datatable(
        data_val,
        options = list(
          rowId = ~Expense.Category,
          pageLength = 20
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        escape = FALSE,
        extensions = 'Buttons',
        selection = 'single'
      ) %>%
        formatStyle(
          'Expense.Category',
          target = 'row',
          selector = 'child',
          fontWeight = styleEqual(c('Total Expenses', 'Total Income', 'Total Save', 'Unidentified'),
                                  c('bold', 'bold', 'bold', 'bold'))
        ) %>%
        formatStyle(
          'Total.Amount',
          target = 'row',
          selector = 'child',
          fontWeight = styleEqual(c('Total Expenses', 'Total Income', 'Total Save', 'Unidentified'),
                                  c('bold', 'bold', 'bold', 'bold'))
        ) %>%
        config(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'font-weight': 'bold'});",
            "}"
          ),
          lengthMenu = list(c(10, 20, 30)),
          pageLength = 20
        )
    }
  })

  output$barChart <- renderPlotly({
    bar_chart_data <- summary_data()
    if (is.null(bar_chart_data)) {
      return(NULL)
    }

    bar_chart_data <- bar_chart_data %>%
      filter(Expense.Category != "Total_Save" & Expense.Category != "Total")

    bar_chart_data$Expense.Category <- factor(bar_chart_data$Expense.Category, levels = bar_chart_data$Expense.Category)

    p <- ggplot(bar_chart_data, aes(x = Expense.Category, y = Total.Amount, fill = Expense.Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Total.Amount), vjust = -0.5, size = 2) +
      labs(title = "Expenses  by Category") +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE) +

      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>% animation_opts(frame = 10000, redraw = TRUE)
  })

  
  
  
 
  output$otherUnidentifiedTable <- renderTable({
    data_val <- filtered_data()
    if (!is.null(data_val)) {
      Unidentified_data <- data_val[data_val$Expense.Category == "Unidentified", ]
      Unidentified_data <- arrange(Unidentified_data, Amount)
      return(Unidentified_data)
    }
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
}




shinyApp(ui, server)
