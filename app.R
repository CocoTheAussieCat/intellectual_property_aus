library(shiny)
library(shinydashboard)
library(jsonlite)
library(httr)
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(RColorBrewer)

patent_attorney_df <- readRDS("patent_attorney_df.rds")

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(
    title = "Intellectual Property Government Open Data", titleWidth = 450
    ),
  dashboardSidebar(
    sidebarMenu(
    id = "tabs",
    menuItem("Patents filed by half year", tabName = "hy", icon = icon("bar-chart")),
    menuItem("Market share per firm", tabName = "mkt_share", icon = icon("line-chart")),
    menuItem("Firm search", tabName = "firm_history", icon = icon("search"))
    )
  ),
 
   dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "hy",
              fluidRow(
                tabBox(id = "tabset1", width = 8,
                            tabPanel("Plot", 
                                     plotOutput("hy_plot")),
                            tabPanel("Table", 
                                     h3(textOutput("year_half_title")),
                                     br(),
                                     tableOutput("hy_table"),
                                     br(),
                                     em(textOutput("data_source")))
                ), # closes tabBox
              box(width = 4,
                  title = "Select number of firms and date",
                  sliderInput("no_firms", 
                              "Number of firms",
                              min = 1,
                              max = 20,
                              value = 10),
                  br(),
                  selectInput("select_yr",
                              "Calendar Year",
                              seq(2018, 1960, by = -1)),
                  radioButtons("select_half",
                               "1H or 2H (calendar year)",
                               c("1H", "2H"))
              ) # closes box
            ) # closes fluidRow
          ), # closes tabItem1
      
      # Second tab content
      tabItem(tabName = "mkt_share",
              fluidRow(
                    box(width = 8,
                      plotOutput("firm_share")
                      ),
                    box(width = 4,
                      title = "Date range",
                      selectInput("start_yr",
                                  "Start Year (calendar)",
                                  seq(2000, 2018, by = 1),
                                  selected = 2016),
                      selectInput("end_yr",
                                  "End Year (calendar)",
                                  seq(2018, 2000, by = -1))
                    ) # closes box
                ) # closes fluidRow
              ), #closes tabItem2
      
      # Third tab content
      tabItem(tabName = "firm_history",
              fluidRow(
                box(width = 8,
                    plotOutput("firm_search")
                ),
                box(width = 4,
                    title = "Firm search",
                    textInput("firm_name", label = "",
                              value = "Watermark")
                )
              ) # closes fluidRow
            ) # closes tabItem #3
      ) # closes tabItems
    ) # closes dashboardBody
   ) # closes dashboardPage


### Define server logic -------------------------------------------------------
server <- (function(input, output) {
  
  input_yr <- reactive({
    input_yr <- paste0(input$select_yr, 
                       if_else(input$select_half == "1H", "-01", "-07"), "-01")
  })
  
  no_firms <- reactive({
    no_firms <- input$no_firms
  })
  
  start_year_plot <- reactive({
    as.Date(paste0(input$start_yr, "-01-01"))
  })
  
  end_year_plot <- reactive({
    as.Date(paste0(input$end_yr, "-07-01"))
  })
  
  firm_name_search <- reactive({
    input$firm_name
  })

### Half year filings plot ------------------------------------------------------------
  output$hy_plot <- renderPlot({
    
    yr <- as.Date(input_yr(), "%Y-%m-%d")
    
    firms <- as.integer(no_firms())
    
    sum_df <- patent_attorney_df %>% 
      filter(filing_half_year == yr) %>% 
      group_by(cleanname, filing_half_year, filing_hy) %>% 
      summarise(no_filings = n()) %>% 
      arrange(desc(no_filings))
    
    sum_df <- sum_df[1:firms, ]
    
    # plot for output 
    ggplot(sum_df) +
      geom_col(aes(x = reorder(cleanname, -no_filings), y = no_filings), fill = "darkblue") +
      labs(x = "", y = "Number of patents filed", 
           title = paste0("Top ", firms, " patent filing companies in ",
                          year(sum_df$filing_half_year), "-", sum_df$filing_hy), 
           caption = "Source: data.gov.au") +
      theme_minimal() +
      theme(plot.title = element_text(color = "black", size = 16, face = "bold")) +
      theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) +
      theme(plot.caption = element_text(size = 12, face = "italic")) +
      theme(axis.text.y = element_text(size = 12))
    
  })

### Half year filings table ------------------------------------------------------------  
  output$hy_table <- renderTable({
    yr <- as.Date(input_yr(), "%Y-%m-%d")
    
    firms <- as.integer(no_firms())
    
    sum_table <- patent_attorney_df %>% 
      filter(filing_half_year == yr) %>% 
      group_by(cleanname, filing_half_year, filing_hy) %>% 
      summarise(no_filings = n()) %>% 
      arrange(desc(no_filings))
    
    # table for output
    sum_table <- sum_table %>% 
      select(filing_half_year, cleanname, no_filings)
    
    names(sum_table) <- c("Year", "Firm", "Total filings")
    
    sum_table <- sum_table[1:firms, 2:3]
    
  })
  
  output$year_half_title <- renderText({
    
    half_text <- ifelse(month(as.Date(input_yr(), "%Y-%m-%d")) == 1, "1H", "2H")
    
    heading_text <- paste0("Top ", as.integer(no_firms()), " patent filing companies in ", 
                           year(as.Date(input_yr(), "%Y-%m-%d")), "-", half_text)
  })
  
  output$data_source <- renderText({
    source_text <- "Source: data.gov.au"
  })
  
### Firm share plot ------------------------------------------------------------
  output$firm_share <- renderPlot({
    firm_df <- patent_attorney_df %>% 
      group_by(firm_grp, filing_half_year, filing_hy) %>% 
      summarise(no_applications = n())
    
    total_app_df <- firm_df %>% 
      group_by(filing_half_year) %>% 
      summarise(total_applications = sum(no_applications)) %>% 
      ungroup(filing_half_year)
    
    firm_df <-  firm_df %>% 
      left_join(total_app_df, by = "filing_half_year") %>% 
      mutate(pct_total = no_applications/total_applications) %>% 
      select(firm_grp, filing_half_year, filing_hy, no_applications, 
             total_applications, pct_total)
    
    start_date <- start_year_plot()
    end_date <- end_year_plot()
    
    plot_df <- firm_df %>% 
      filter(filing_half_year >= start_date &
               filing_half_year <= end_date) %>% 
      mutate(hy_label = paste0(year(filing_half_year), "-", filing_hy))
    
    plot_df %>% 
      ggplot() +
      geom_col(aes(x = hy_label, y = pct_total, fill = firm_grp)) +
      theme_minimal() +
      labs(title = "Percent of patent applications by firm", 
           x = "", 
           y = "", 
           caption = "Source: data.gov.au") +
      scale_color_brewer(palette = "Spectral") +
      theme(legend.position = "right", legend.title = element_blank()) +
      scale_y_continuous(labels = percent_format(accuracy = 1, big.mark = ",")) +
      theme(plot.caption = element_text(face = "italic", size = 12)) +
      theme(plot.title = element_text(face = "bold", size = 16)) +
      theme(axis.text.x = element_text(angle = 90, size = 12)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(legend.text = element_text(size = 12))
  })

### Search for single attorney name and plot number of filings in each six month period-----
  output$firm_search <- renderPlot({
  
    search_name <- firm_name_search()
    
    search_df <- patent_attorney_df %>% 
      filter(grepl(search_name, cleanname, ignore.case = T)) %>% 
      group_by(cleanname, filing_half_year, filing_hy) %>% 
      summarise(no_applications = n()) %>% 
      arrange(filing_half_year)
    
    search_df %>% 
      ggplot() +
      geom_col(aes(x = filing_half_year, y = no_applications, fill = filing_hy)) +
      labs(x = "", y = "Number of patents filed", 
           title = paste0(search_df$cleanname[1], " patents filed"),
           caption = "Source: data.gov.au") +
      theme_minimal() +
      scale_y_continuous() +
      scale_x_date(breaks = waiver()) +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "right", legend.title = element_blank()) +
      theme(plot.title = element_text(color = "black", size = 16, face = "bold")) +
      theme(axis.text.x = element_text(size = 12)) +
      theme(plot.caption = element_text(size = 12, face = "italic")) +
      theme(axis.text.y = element_text(size = 12))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
