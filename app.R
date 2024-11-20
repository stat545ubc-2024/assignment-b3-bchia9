# Load Packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(DT)

# Read and re-classify dataset
kpi_sp500 <- read.csv("sp500-kpis-2024.csv", 
                           colClasses = c("character", "character", "character", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "character"),
                           na.strings = c("null"))

# Renaming columns to match stringr::str_replace...() pattern recognition
kpi_sp500 <- kpi_sp500 %>%
  rename("Earnings.per.Share"= Earnings.Share, "Week.52.Low" = X52.Week.Low, "Week.52.High" = X52.Week.High,
         "Price.to.Earnings" = Price.Earnings, "Price.to.Book" = Price.Book, "Price.to.Sales" = Price.Sales)

# Global UI Features
ui <- fluidPage(

    titlePanel("Key Performance Indicators (KPI's) of S&P 500 Companies"),
    h4(em("(Updated November 2024)")),
    br(),
    
    h5("Welcome! This app contains a comprehensive dataset of the key performance indicators (KPIs) for the S&P 500 companies.
       If you navigate to the panel tabs, you can visualize the relationship between KPIs of a company and compare performance between companies by selecting a sector(s) and company by name(s) or ticker symbol(s).
       You can also find a data table containing the KPIs for each company, which can be filtered by sector."),
    br(),
    h6("Credit to Rufus Pollock and Open Knowledge Foundation for providing this dataset."),
    
    br(),
    br(),
    
# UI Features for Tab 1

# FEATURE 1: The `tabsetPanel()` feature of this app allows the user to interactively switch between a dashboard containing a scatter plot, which can be modified by user selection of filter conditions in the side panel, and a dashboard with a table view of the data with its own filter conditions in a unique side panel.
    tabsetPanel(
      tabPanel("KPI Scatter Plot", "",
    sidebarLayout(
# FEATURE 2: The selectInput() function allows for a multiple selection input, which is useful because it gives the user freedom to compare data from multiple different companies and sectors. Users are also able to select companies based on their name or ticker symbol with two parallel selectInput() functions (using companyInput or symbolInput) using reactive programming in a code below. This is useful because an app-user may be familiar with only the company name or ticker symbol, and vice versa.
        sidebarPanel(
          h5(strong("1. Compare the performance of S&P 500 companies by their KPIs.")),
          h6("Select"),
          selectInput("sectorInput", "Sector(s)",
                      choices = kpi_sp500$Sector,
                      multiple = TRUE),
          h6("And"),
          selectInput("companyInput", "Company Name(s)",
                      choices = kpi_sp500$Name,
                      multiple = TRUE),
          h6("Or"),
        selectInput("symbolInput", "Ticker Symbol(s)",
                    choices = kpi_sp500$Symbol,
                    multiple = TRUE),
        br(),br(),
        h5(strong("2. Choose two KPIs for comparison.")),
        selectInput("xInputKPI", "X Input",
                     choices = names(kpi_sp500)[sapply(kpi_sp500, is.numeric)]),
        selectInput("yInputKPI", "Y Input",
                    choices = rev(names(kpi_sp500)[sapply(kpi_sp500, is.numeric)]))
        ), 
        
                    
                                       
        mainPanel(plotOutput("kpiplot"))
)
),

# UI features for Tab 2
tabPanel("KPI Table", "",
         sidebarLayout(
           sidebarPanel(
             h6("Filter by:"),
             selectInput("sectorInput2", "Sector(s)",
                         choices = kpi_sp500$Sector,
                         multiple = TRUE),
             h6("Either:"),
             selectInput("companyInput2", "Company Name(s)",
                         choices = kpi_sp500$Name,
                         multiple = TRUE),
             h6("Or"),
             selectInput("symbolInput2", "Ticker Symbol(s)",
                         choices = kpi_sp500$Symbol,
                         multiple = TRUE),
           ), 
           
           
           
           mainPanel(DTOutput("kpitable"))
)
)
)
)

server <- function(input, output, session) {

# Start of Tab 1 server function ---------------------------------------
  
  # Reactive element to filter company name input options by sector 
  observeEvent(input$sectorInput, {
    choice_sectors <- input$sectorInput
    filtered_companies <- kpi_sp500 %>%
      filter(Sector %in% choice_sectors) %>%
      select(Name)
    
    updateSelectInput(session, "companyInput",
                      choices = filtered_companies$Name)
    
    updateSelectInput(session, "symbolInput",
                      choices = NULL)
  })
  
  # Reactive element to filter ticker symbol input options by sector
  observeEvent(input$sectorInput, {
      choice_sectors <- input$sectorInput
    filtered_symbols <- kpi_sp500 %>%
      filter(Sector %in% choice_sectors) %>%
      select(Symbol)
    
    updateSelectInput(session, "symbolInput",
                      choices = filtered_symbols$Symbol)
  })
  
  # Reactive element to filter ticker symbol input by company name
  observeEvent(input$companyInput, {
    choice_companies <- input$companyInput
    filt_sym_by_comp <- kpi_sp500 %>%
      filter(Name %in% choice_companies) %>%
      select(Symbol)
    
    updateSelectInput(session, "symbolInput",
                      choices = filt_sym_by_comp$Symbol)
  })
  
  
  # Reactive filtering based on the selected kpi inputs
  filtered_inputs <- reactive({
    req(
      (length(input$sectorInput) > 0 & length(input$companyInput) > 0) |
      (length(input$sectorInput) > 0 & length(input$symbolInput) > 0)
    )
   data <- kpi_sp500 %>%
      filter(
        (Sector %in% input$sectorInput | is.null(input$sectorInput)),
        (Name %in% input$companyInput | is.null(input$companyInput)),
        (Symbol %in% input$symbolInput | is.null(input$symbolInput))
      )
    
   # Transformation of EDITDA and Market.Cap data by scaling down x1000
   if(input$xInputKPI == "EBITDA") {
     data$EBITDA <- data$EBITDA/1000
   }
   if(input$yInputKPI == "EBITDA") {
     data$EBITDA <- data$EBITDA/1000
   }
   
   if(input$xInputKPI == "Market.Cap") {
     data$Market.Cap <- data$Market.Cap/1000
   }
   if(input$yInputKPI == "Market.Cap") {
     data$Market.Cap <- data$Market.Cap/1000
   }
   
   return(data)
  })
  
  # Render scatter plot
  output$kpiplot <- renderPlot({
    req(input$xInputKPI, input$yInputKPI)
    
    plot <- ggplot(filtered_inputs(), aes_string(x = input$xInputKPI, y = input$yInputKPI)) +
      geom_point(size = 1) +
      geom_text(aes(label = Symbol), nudge_x = 0, nudge_y = 0, check_overlap = T) +
      labs(
        x = case_when(
          input$xInputKPI %in% c("EBITDA", "Market.Cap") ~ paste(str_replace_all(input$xInputKPI, "\\.", " "), "(x $1000)"),
          str_detect(input$xInputKPI, "\\.") ~ str_replace_all(input$xInputKPI, "\\.", " "),
          TRUE ~ input$xInputKPI
        ),
        y = case_when(
          input$yInputKPI %in% c("EBITDA", "Market.Cap") ~ paste(str_replace_all(input$yInputKPI, "\\.", " "), "(x $1000)"),
          str_detect(input$yInputKPI, "\\.") ~ str_replace_all(input$yInputKPI, "\\.", " "),
          TRUE ~ input$yInputKPI
        ),
        title = paste("Scatter plot of",
                      str_replace_all(input$xInputKPI, "\\.", " "),
                      "vs.",
                      str_replace_all(input$yInputKPI, "\\.", " "))
      ) +
      theme_cowplot()
    
    if (input$xInputKPI %in% c("EBITDA", "Market.Cap")) {
      plot <- plot + scale_x_continuous(labels = scales::label_number(scale = 1/1000, accuracy = 0.01))
    } 
    else if (input$xInputKPI %in% c("Price.to.Earnings", "Dividend.Yield", "Price.to.Sales", "Price.to.Book")) {
        plot <- plot + scale_x_continuous(labels = scales::label_number(accuracy = 0.01))
    }
    else {
      plot <- plot + scale_x_continuous(labels = scales::label_number(accuracy = 0.01))
    }
    if (input$yInputKPI %in% c("EBITDA", "Market.Cap")) {
      plot <- plot + scale_y_continuous(labels = scales::label_number(scale = 1/1000, accuracy = 0.01))
    }
    else if (input$yInputKPI %in% c("Price.to.Earnings", "Dividend.Yield", "Price.to.Sales", "Price.to.Book")) {
      plot <- plot + scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
    }
    else {
      plot <- plot + scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
    }
    
    plot
  })
  
# End of Tab 1 server function -----------------------------------

# Start of Tab 2 server function ----------------------------------

# Reactive element to filter company name input options by sector 
observeEvent(input$sectorInput2, {
  choice_sectors2 <- input$sectorInput2
  filtered_companies2 <- kpi_sp500 %>%
    filter(Sector %in% choice_sectors2) %>%
    select(Name)
  
  updateSelectInput(session, "companyInput2",
                    choices = filtered_companies2$Name)
  
  updateSelectInput(session, "symbolInput2",
                    choices = NULL)
})

# Reactive element to filter ticker symbol input options by sector 
observeEvent(input$sectorInput2, {
  choice_sectors2 <- input$sectorInput2
  filtered_symbols2 <- kpi_sp500 %>%
    filter(Sector %in% choice_sectors2) %>%
    select(Symbol)
  
  updateSelectInput(session, "symbolInput2",
                    choices = filtered_symbols2$Symbol)
})

# Reactive element to filter ticker symbol input by company name
observeEvent(input$companyInput2, {
  choice_companies2 <- input$companyInput2
  filt_sym_by_comp2 <- kpi_sp500 %>%
    filter(Name %in% choice_companies2) %>%
    select(Symbol)
  
  updateSelectInput(session, "symbolInput2",
                    choices = filt_sym_by_comp2$Symbol)
})

# FEATURE 3: The `DT::renderDataTable()` function allows the user to interact with the table output by sorting numeric columns in ascending/descending order and character columns alphabetically, which is useful if the user wishes to explore each column based on a specific order (e.g., highest market cap) instead of searching for specific companies.
output$kpitable <- DT::renderDataTable({
  filtered_table <- kpi_sp500 %>%
    filter(
      (Sector %in% input$sectorInput2 | is.null(input$sectorInput2)),
      (Name %in% input$companyInput2 | is.null(input$companyInput2)),
      (Symbol %in% input$symbolInput2 | is.null(input$symbolInput2))
    ) %>%
    rename(
      "P/E Ratio" = "Price.to.Earnings",
      "Dividend Yield (%)" = "Dividend.Yield",
      "EPS ($)" = "Earnings.per.Share",
      "52-Week Low ($)" = "Week.52.Low",
      "52-Week High ($)" = "Week.52.High",
      "Market Cap ($)" = "Market.Cap",
      "EBITDA ($)" = "EBITDA",
      "P/S Ratio" = "Price.to.Sales",
      "P/B Ratio" = "Price.to.Book", 
      "SEC Filings" = "SEC.Filings"
           )
  datatable(
    filtered_table,
    options = list(
      autoWidth = TRUE, 
      pageLength = 10,  
      order = list(list(0, 'asc')) 
    ),
  )
})

# End of Tab 2 server function ------------------------------------
    
}

# Run the application 
shinyApp(ui = ui, server = server)
