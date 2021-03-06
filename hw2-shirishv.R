# importing required libraries

# install.packages("bindrcpp")
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

# install.packages(ggmap)
# install.packages("dashboardthemes")

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tools)
library(ggmap)

register_google(key = "AIzaSyDGLYamQykg8ejoSKVfnLVO4zMyX_Mi8Bw")

# loading the Park and Ride dataset

pnr <- read.csv("Park_Ride_Database_2020.csv")
colnames(pnr)[11] <- "Average.Use.2020"
colnames(pnr)[12] <- "Average.Utilization"

# To avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application title ----------------------------------------------
header <- dashboardHeader(title = "PAAC Park & Ride Dashboard", titleWidth = 300)

# Dashboard sidebar
sidebar <- dashboardSidebar(width = 300,
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
                "Total # parking lots/spaces:",
                choices = c("Count of Lots", "Count of Parking Spaces"),
                selected = "Count of Lots"),
    
    # Input for x-axis of the plot
    selectInput("Location",
                "Group Park & Rides by:",
                choices = c("Region","Municipality","Ownership",
                            "General Ownership" = "General.Ownership"),
                selected = "Region"),
    
    # Input for lower and upper value of utilization for medium utilization
    sliderInput(inputId = "slider", 
                label = "Select the range of utilization for medium performance (orange):", 
                min = 0, 
                max = 1,
                step = 0.05, 
                value = c(0.40,0.75)),
    
    # Checkbox to filter lots in status bar
    uiOutput("filter")
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Value Boxes ----------------------------------------------
          fluidRow(valueBoxOutput("lots"),
                   valueBoxOutput("spaces"),
                   valueBoxOutput("utilization")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plots",
                   width = 12,
                   tabPanel(strong("How is the distribution of Park & Ride lots look like?"), plotlyOutput("barplot")),
                   tabPanel(strong("What are lots' utilization rates?"), plotlyOutput("statusbar", height = "700")),
                   tabPanel(strong("Where are all the lots located?"), plotlyOutput("map", height = "70vh")))
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
ui <- dashboardPage(skin = "green", header, sidebar, body)

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
    
    valueBox(subtitle = "Total lots", value = num, icon = icon("parking"), color = "blue")
  })
  
  output$spaces <- renderValueBox({
    num <- sum(pnr$Capacity)
    
    valueBox(subtitle = "Total spaces", value = num, icon = icon("car-side"), color = "blue")
  })
  
  output$utilization <- renderValueBox({
    num <- pnr %>%
      summarise(paste(round(sum(Average.Use.2020)*100/sum(Capacity),2),"%"))
    
    valueBox(subtitle = "Monthy average utilization", value = num, icon = icon("chart-bar"), color = "blue")
  })
  
  # Making a bar plot to show count of lots across different geographic divisions
  output$barplot <- renderPlotly({
    ggplot(pnrInput.new(), aes_string(x = input$Location, y = make.names(input$Metric), fill = input$Location)) +
      geom_bar(stat = "identity") +
      labs(x = gsub("\\.", " ", input$Location), y = gsub("\\.", " ", input$Metric),
           fill = gsub("\\.", " ", input$Location)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = ifelse(input$Location == "Municipality",45,0))) +
      theme(legend.title = element_text(size = ifelse(input$Location == "General.Ownership", 8, 10))) +
      theme(axis.title.x = element_text(face = "bold"), 
            axis.title.y = element_text(face = "bold"))
  })
  
  # Creating a checkbox UI to place in sidebar using inputs
  output$filter <- renderUI({
    checkboxGroupInput("filter",
                       "Filter lot utilization status bar using:",
                       choices = unique(pnr[,input$Location]),
                       selected = unique(pnr[,input$Location]))
  })
  
  # Creating a subset based on the checkbox
  filtered.df <- reactive({
    req(input$filter)
    pnr[pnr[,input$Location] %in% input$filter, ]
  })
  
  # Making a status bar type bar plot showing utilization rate of each parking lot
  output$statusbar <- renderPlotly({
      ggplot(data = filtered.df()) +
        geom_bar(stat = "identity", fill = "#eeeeee", aes(x = Name, y = 1)) +
        geom_bar(stat = "identity", 
                 fill = ifelse(filtered.df()$Average.Utilization >= input$slider[2], "#2dc937", 
                               ifelse(filtered.df()$Average.Utilization >= input$slider[1], "#e7b416", "#cc3232")), 
                 aes(x = Name, y = Average.Utilization)) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Parking Lot Names", y = " Average Monthly Utilization Rate in 2020") +
        theme(axis.title.x = element_text(face = "bold"), 
              axis.title.y = element_text(face = "bold")) +
        coord_flip()
  })
  
  # Plotting map containing locations of Park & Rides
  output$map <- renderPlotly({
    map <- get_map(location = "pittsburgh", zoom = 10, source = "google", maptype = "roadmap")
    ggmap(map) +
      geom_point(data = filtered.df(), aes_string(x = "Longitude", y = "Latitude", color = input$Location), size = 2) +
      labs(x = "Longitude", y = "Latitude") +
      theme(axis.title.x = element_text(face = "bold"), 
            axis.title.y = element_text(face = "bold"))
  })
  
  
  # Aggregated Data table ----------------------------------------------
  output$agg.table <- DT::renderDataTable({pnrInput.new()})
  
  # Raw Data table ----------------------------------------------
  output$raw.table <- DT::renderDataTable({pnr[2:13]})
}

# Running the application ----------------------------------------------
shinyApp(ui = ui, server = server)
