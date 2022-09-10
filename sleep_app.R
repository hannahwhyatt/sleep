

library(lubridate)
library(shiny)
library(hms)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)


## Load and format the data ====================================================

# Load the data ----------------------------------------------------------------
load('sleep_data.Rdata')
sleep <- proportionalSleep

# Format and select data to display in a graph ---------------------------------

# Bedtimes must include a date: before and after midnight of the same night
sleep$timing <- as.POSIXct(sleep$timing)
sleep$total_sleep_hms <- hms::hms(sleep$total_sleep) %>% as.POSIXct()

# Select the variables to be displayed
sleep <- select(sleep, 
                "deep_sleep_p", "light_sleep_p", "REM_p", "timing", 
                "total_sleep_hms")

# Create more user-friendly variable names
colnames(sleep) <- c("deep sleep", "light sleep", "REM sleep", "bedtime", 
                     "duration")

# Find earliest and latest bedtimes for the x-axis 
minTime <- min(sleep$bedtime) %>% lubridate::ymd_hms()
maxTime <- max(sleep$bedtime) %>% lubridate::ymd_hms()


# Format the data to be displayed in a table -----------------------------------

# Create a copy of the data for displaying as a table
sleep_tab <- sleep

# 'bedtime' and duration of sleep should only display hours/minutes/seconds
sleep_tab$bedtime <- hms::as_hms(sleep_tab$bedtime)
sleep_tab$duration <- hms::as_hms(sleep_tab$duration)

# Sleep stage values are rounded for readability
sleep_tab[1:3] <- round(sleep_tab[1:3], digits = 2)



## App =========================================================================

# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  tabsetPanel(
    tabPanel('Graph',
             
             titlePanel('REM Sleep: Jan-Aug 2022'),
             
             fluidRow(
               column(5,
                      h3('', align = 'left'),
                      plotOutput('REM_plot', click = 'plot_click')
                      ),
               column(4,
                      titlePanel(" "),
                      
                      sliderInput('REM_time', 
                                  "Filter by duration of sleep (hr/min/sec):", 
                                  min = min(sleep$duration), 
                                  max = max(sleep$duration),
                                  value = c(min(sleep$duration),
                                            max(sleep$duration)),
                                  timeFormat = "%H:%m:%S"
                                  ),
                      
                      conditionalPanel(condition = "!input.plot_click",
                                       textOutput('init_message')
                                       ),
                      
                      conditionalPanel(condition = "input.plot_click",
                                       dataTableOutput('sleep_details')
                                       )
                      ),
               
               column(3)
               )
             ),
    
    tabPanel('Table',
             dataTableOutput('table'))
    
    
  )
)

# Server
# ------------------------------------------------------------------------------

server <- function(input, output) {
  
  thematic::thematic_shiny()
  
  selected_night <- reactive({
    
    night <- nearPoints(sleep, input$plot_click, maxpoints = 1) %>% 
      as.data.frame()
    
    night <- as.data.frame(night)
  })
  
  output$sleep_details <- renderDataTable({
    
    t1 <- selected_night() 
    
    t1$bedtime <- as_hms(t1$bedtime) %>% as.character()
    t1$duration <- as_hms(t1$duration)  %>% as.character()
    
    t1 <-  pivot_longer(t1,
                        everything(),
                        names_to = "Metric",
                        values_to = "Value",
                        values_transform = as.character) %>%
      DT::datatable(options = list(dom = 't'), 
                    rownames = FALSE) %>%
      formatPercentage('Value', rows = 1:3, digits = 0) 
    
  })
  
  output$init_message <- renderText({
    HTML("Click on a point to view sleep metrics.")
  }
  )
  
  output$REM_plot <- renderPlot({
    
    selectedRows <- sleep$duration >= input$REM_time[1] & 
      sleep$duration <= input$REM_time[2]
    
    plotData <- sleep[selectedRows ,]
    
    p1 <- ggplot(plotData, aes(x = bedtime, y = `REM sleep`)) +
      geom_point(colour = 'dodgerblue4') +
      xlab("Bedtime") +
      scale_y_continuous(name = "Proportion of sleep spent in REM", 
                         limits = c(0, 1)) +
      scale_x_datetime(limits = c(minTime, maxTime)) +                                  
      theme(axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 14)) +
      geom_smooth(method = lm, se = FALSE) 
    
    p1
  })
  
  output$table <- renderDataTable(
    
    DT::datatable(sleep_tab,
                  rownames = FALSE,
                  options = list(dom = 'Brtip', autoWidth = FALSE)
                  ) 
    
  )
  
}

# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
