# importing required libraries

# installing/loading the package
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
}

# using the package bcz shiny, shiythemes were built on R 3.6.3
# updateR()

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tools)

# loading the Park and Ride dataset

pnr <- read.csv("Park_Ride_Database_2020.csv")
colnames(pnr)[11] <- "Average.Use.2020"
colnames(pnr)[12] <- "Average.Utilization"

# To avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application title ----------------------------------------------
header <- dashboardHeader(title = "Port Authority Park & Ride Dashboard")

# Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem(text ="Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem(text = "Table based on inputs", icon = icon("table"), tabName = "agg_table"),
    menuItem(text = "Raw table", icon = icon("table"), tabName = "raw_table"),
    
    # Adding a horizontal line to separate tabs with inputs section
    hr(),
    
    # Inputs: selecting variables to plot ------------------------------
    # Input for y-axis of the plot
    selectInput("Metric",
                "Total # lots/spaces:",
                choices = c("Count of Lots", "Count of Parking Spaces"),
                selected = "Count of Lots"),
    
    # Input for x-axis of the plot
    selectInput("Location",
                "Group Park & Rides by:",
                choices = c("Region","Municipality","Ownership",
                            "General Ownership" = "General.Ownership"),
                selected = "Region")
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Value Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("lots"),
            valueBoxOutput("spaces"),
            valueBoxOutput("utilization")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plots",
                   width = 12,
                   tabPanel("Distribution of Park & Ride lots", plotlyOutput("barplot")),
                   tabPanel("Lots Utilization", plotlyOutput("plot_height")))
          )
  ),
  
  # Aggregated data Table Page ----------------------------------------------
  tabItem("agg_table",
          fluidPage(
            box(title = "Aggregated data (based on inputs)", 
                DT::dataTableOutput("agg.table"), width = 12))
  ),
  
  # Raw data Table Page ----------------------------------------------
  tabItem("raw_table",
          fluidPage(
            box(title = "Raw data", DT::dataTableOutput("raw.table"), width = 12))
  )
)
)

# UI
ui <- dashboardPage(header, sidebar, body)

# Defining server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Aggregating the dataset based on the selected inputs
  pnrInput <- reactive({
    pnr.agg <- pnr %>%
      group_by(get(input$Location)) %>%
      summarise(values = ifelse(input$Metric == "Count of Lots", n(), sum(Capacity)))
    
    return(pnr.agg)
  })
  
  # changing names of the columns
  pnrInput.new <- reactive({
    agg.data <- pnrInput()
    colnames(agg.data) <- c(input$Location, make.names(input$Metric))
    
    return(agg.data)
  })
  
  # Making three valueboxes
  output$lots <- renderValueBox({
    num <- nrow(pnr)
    
    valueBox(subtitle = "Total lots", value = num, icon = icon("parking"), color = "orange")
  })
  
  output$spaces <- renderValueBox({
    num <- sum(pnr$Capacity)
    
    valueBox(subtitle = "Total spaces", value = num, icon = icon("car-side"), color = "orange")
  })
  
  output$utilization <- renderValueBox({
    num <- pnr %>%
      summarise(paste(round(sum(Average.Use.2020)*100/sum(Capacity),2),"%"))
    
    valueBox(subtitle = "Monthy average utilization", value = num, icon = icon("hourglass-half"), color = "orange")
  })
  
  # Making a bar plot to show count of lots across different geographic divisions
  output$barplot <- renderPlotly({
    ggplot(pnrInput.new(), aes_string(x = input$Location, y = make.names(input$Metric), fill = input$Location)) +
      geom_bar(stat = "identity") +
      labs(x = gsub("\\.", " ", input$Location), y = gsub("\\.", " ", input$Metric),
           fill = gsub("\\.", " ", input$Location)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = ifelse(input$Location == "Municipality",45,0))) +
      theme(legend.title = element_text(size = ifelse(input$Location == "General.Ownership", 8, 10)))
  })
  
  # Aggregated Data table ----------------------------------------------
  output$agg.table <- DT::renderDataTable({pnrInput.new()})
  
  # Raw Data table ----------------------------------------------
  output$raw.table <- DT::renderDataTable({pnr[2:13]})
}

# Running the application ----------------------------------------------
shinyApp(ui = ui, server = server)
