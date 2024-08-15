#install.packages("shiny")
#install.packages("plotly")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("shinyWidgets")
#install.packages("writexl")
#install.packages("rmarkdown")
#install.packages("webshot2")
#install.packages("shinyjs")  # For user authentication
#install.packages("shinymanager")  # For authentication



#update.packages("shiny")



library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyWidgets)
library(writexl)
library(rmarkdown)
library(webshot2)  # For saving plots as images
library(rmarkdown)  # For saving plots as PDF
library(shinyjs)  # For user authentication
library(shinymanager)  # For authentication


# Define UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("mtcars Data Visualization"),
  
  # Main UI
  div(id = "main-content", style = "display:none;",
      sidebarLayout(
        sidebarPanel(
          
          # Dropdown menu for x-axis variable selection
          selectInput("xvar", 
                  "Select X-axis variable:",
                  choices = names(mtcars),
                  selected = "drat"),
      
          # Dropdown menu for y-axis variable selection
          selectInput("yvar", 
                  "Select Y-axis variable:",
                  choices = names(mtcars),
                  selected = "mpg"),
      
          # Slider to filter data by the number of cylinders
          sliderInput("cylinders", 
                  "Number of cylinders:",
                  min = min(mtcars$cyl),
                  max = max(mtcars$cyl),
                  value = range(mtcars$cyl),
                  step = 2),
      
          # Slider to filter data by horsepower range
          sliderInput("hpRange", 
                  "Horsepower range:",
                  min = min(mtcars$hp),
                  max = max(mtcars$hp),
                  value = c(min(mtcars$hp), max(mtcars$hp))),
      
          # Dropdown menu for transmission type
          selectInput("transmission", 
                  "Transmission type:",
                  choices = c("All", "Automatic", "Manual"),
                  selected = "All"),
      
          # Checkbox to include/exclude regression line
          checkboxInput("regression", 
                    "Add regression line", 
                    value = FALSE),
      
          # Dropdown menu for plot type selection
          selectInput("plotType",
                  "Select Plot Type:",
                  choices = c("Scatter Plot" = "scatter",
                              "Line Plot" = "line",
                              "Bar Plot" = "bar"),
                  selected = "scatter"),
      
          # Color and size customization
          textInput("pointColor", "Point Color:", value = "blue"),
          sliderInput("pointSize", "Point Size:", min = 1, max = 10, value = 3),
      
          # Variable transformation options
          selectInput("xTrans", 
                  "Transform X-axis variable:",
                  choices = c("None", "Log", "Square Root"),
                  
                  selected = "None"),
          selectInput("yTrans", 
                  "Transform Y-axis variable:",
                  choices = c("None", "Log", "Square Root"),
                  selected = "None"),
          
          # Statistical tests selection
          selectInput("statTest", 
                      "Select Statistical Test:", 
                      choices = c("None", "T-test", "ANOVA"),
                      selected = "None"),
      
          # Format selection and download button for data
          selectInput("dataFormat", 
                  "Select Data File Format:", 
                  choices = c("CSV", "Excel")),
          downloadButton("downloadData", "Download Data"),
      
          # Format selection and download button for plot
          selectInput("plotFormat",
                  "Select Plot File Format:",
                  choices = c("PNG", "PDF")),
          downloadButton("downloadPlot", "Download Plot")
        ),
    
          # Main panel for displaying outputs
        mainPanel(
          plotlyOutput("scatterPlot"),
          h3("Selected Data Table"),  # Header for selected data table
          tableOutput("summaryTable"),  # Display summary table
          verbatimTextOutput("statTestResult")  # Output for statistical test results
        )
     )
  )
)

# Define server logic
server <- function(input, output) {
  
  # User Authentication
  auth <- secure_server(
    check_credentials = check_credentials(data.frame(
      user = c("user1", "user2"),
      password = c("pass1", "pass2"),
      stringsAsFactors = FALSE
    ))
  )
  
  observe({
    req(auth)
    shinyjs::show("main-content")
  })
  
  # Reactive expression to filter data based on the slider input
  filteredData <- reactive({
    data <- mtcars %>%
      filter(cyl %in% input$cylinders,
             hp >= input$hpRange[1],
             hp <= input$hpRange[2])
    
    if (input$transmission == "Automatic") {
      data <- data %>% filter(am == 0)
    } else if (input$transmission == "Manual") {
      data <- data %>% filter(am == 1)
    }
    
    if (input$xTrans == "Log" && input$xvar != "cyl") {
      data[[input$xvar]] <- log(data[[input$xvar]])
    } else if (input$xTrans == "Square Root" && input$xvar != "cyl") {
      data[[input$xvar]] <- sqrt(data[[input$xvar]])
    }
    
    if (input$yTrans == "Log" && input$yvar != "cyl") {
      data[[input$yvar]] <- log(data[[input$yvar]])
    } else if (input$yTrans == "Square Root" && input$yvar != "cyl") {
      data[[input$yvar]] <- sqrt(data[[input$yvar]])
    }
    
    data
  })
  
  # Perform the selected statistical test
  output$statTestResult <- renderPrint({
    data <- filteredData()
    
    if (input$statTest == "T-test" && length(unique(data$cyl)) == 2) {
      t.test(data[[input$xvar]], data[[input$yvar]])  # Perform T-test
    } else if (input$statTest == "ANOVA") {
      aov(data[[input$yvar]] ~ data[[input$xvar]], data = data) %>%
        summary()  # Perform ANOVA
    } else {
      "No statistical test selected or appropriate."
    }
  })
  
  # Render the scatter plot with tooltips and hover effects
  output$scatterPlot <- renderPlotly({
    data <- filteredData()
    
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
      theme_minimal() +
      geom_point(aes(text = paste(input$xvar, ":", data[[input$xvar]], 
                                  "<br>", input$yvar, ":", data[[input$yvar]])),
                 color = input$pointColor, size = input$pointSize) +
      labs(x = input$xvar, y = input$yvar)
    
    if (input$regression) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    if (input$plotType == "line") {
      p <- p + geom_line()
    } else if (input$plotType == "bar") {
      p <- p + geom_bar(stat = "identity")
    }
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  # Render the summary table showing only the selected columns for plotting
  output$summaryTable <- renderTable({
    data <- filteredData()
    data %>%
      select(all_of(c(input$xvar, input$yvar)))
  })
  
  
  # Download the filtered data based on selected format
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$dataFormat == "CSV") {
        paste("filtered_data-", Sys.Date(), ".csv", sep = "")
      } else if (input$dataFormat == "Excel") {
        paste("filtered_data-", Sys.Date(), ".xlsx", sep = "")
      }
    },
    content = function(file) {
      data <- filteredData()
      dataToDownload <- data %>%
        select(all_of(c(input$xvar, input$yvar)))
      
      if (input$dataFormat == "CSV") {
        write_csv(dataToDownload, file)
      } else if (input$dataFormat == "Excel") {
        writexl::write_xlsx(dataToDownload, file)
      }
    }
  )
  
  # Download the plot based on selected format
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".", tolower(input$plotFormat), sep = "")
    },
    content = function(file) {
      data <- filteredData()
      p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
        theme_minimal() +
        geom_point(color = input$pointColor, size = input$pointSize) +
        labs(x = input$xvar, y = input$yvar)
      
      if (input$regression) {
        p <- p + geom_smooth(method = "lm", se = FALSE)
      }
      
      if (input$plotType == "line") {
        p <- p + geom_line()
      } else if (input$plotType == "bar") {
        p <- p + geom_bar(stat = "identity")
      }
      
      if (input$plotFormat == "PNG") {
        ggsave(file, plot = p, device = "png")
      } else if (input$plotFormat == "PDF") {
        ggsave(file, plot = p, device = "pdf")
      }
    }
  )
}



# Run application
shinyApp(ui = ui, server = server)