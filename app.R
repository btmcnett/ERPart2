

library(shiny)
library(vroom)
library(tidyverse)
library(shinythemes)
library(forcats)


injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population




# gets code for product filtering
prod_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  # sets theme
  theme = shinytheme("journal"),
  h1("ER Visit Dashboard"),
  p("This app allows users to observe data of ER Visits in 2017. The data is from the National Electronic Injury Surveillance System (NEISS). There is a sidebar with 3 selection options, corresponding to the product associated with the injury, the number of unique rows that will appear in the resulting tables, and the options for the y axis on the plot below the tables. Underneath the plot, there will be multiple narrative options."),
  hr(),
  
  sidebarLayout(
  # First row containing the product, selecting code and title
   sidebarPanel(selectInput("code", "Product",
                           choices = setNames(products$prod_code, products$title),
                           width = "100%"
  ),
  
  numericInput(inputId = "nRows", label = "Number of unique rows",
               min = 2, max = 20, step = 1, value = 5),
  
  selectInput("y", "Y axis", c("rate", "count"))
  
    
  ),
  
  mainPanel(
    fluidRow(
      column(4, tableOutput("diag")),
      column(4, tableOutput("body_part")),
      column(4, tableOutput("location")),
      
    ) # closes fluid row
    ) # closes main panel
  ), # closes sidebar
    
    hr(),
  
  h3("Rate/Count of Injuries over Age and Sex"),
  p("The graph details the count or rate associated with the product as a function of the age of the patient, with sex being assigned to the color."),
  
    
    fluidRow(
      # Third row containing the plot
      column(12, plotOutput("age_sex"))
    ),
  
  hr(),
  
  h3("Frequency of Injuries over the Year"),
  p("The graph details the monthly sum of injuries over the entire year."),
  
  
  fluidRow(
    # Third row containing the plot
    column(12, plotOutput("time"))
  ),
      
    
  
  
  

  hr(),
  # Fourth row containing the action button that displays a random narrative with the selected product
  
  h3("Injury Descriptions"),
  p("There are two sections which detail different selection methods for descriptions of the injury. The first will select a random story associated with the product, while the second will allow the user to scroll between all the narratives associated with that injury."),
  
  fluidRow(
    column(2, actionButton("story", "Tell me a story", class="btn btn-primary")),
    column(10, textOutput("narrative"))
  ),
  
hr(),
  
  # Fifth row containing two action buttons and narratives.
  fluidRow(
    column(2, actionButton("prevStory", "Previous Narrative")),
    column(8, textOutput("narrativeMiddle")),
    column(2, actionButton("nextStory", "Next Narrative"))
  )
)

# factor lumping function
# function of a data frame, variable, and number of rows
count_top <- function(df, var, n) {
  df %>%
    # mutate the variable so it lumps all levels except for other categories
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{ n }},
                                 other_level = "Sum of All Other Categories")) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  # converts selected injuries into a reactive element
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # output table for diagnoses - sorts greatest to lowest
  output$diag <- renderTable({
    diagTable <- count_top(df = selected(), var = diag, n = input$nRows - 1)
    colnames(diagTable) <- c("Diagnosis", "Number")
    diagTable
    
    #selected() %>% count(diag, wt = weight, sort = TRUE)
  }, width = "100%" )
  
  
  # output table for body part - sorts greatest to lowest
  output$body_part <- renderTable({
    bodyTable <- count_top(df = selected(), var = body_part, n = input$nRows - 1)
    colnames(bodyTable) <- c("Body part injured", "Number")
    bodyTable
    
    #selected() %>% count(diag, wt = weight, sort = TRUE)
  }, width = "100%" )
  
  
  # output table for location - sorts greatest to lowest
  output$location <- renderTable({
    locationTable <- count_top(df = selected(), var = location, n = input$nRows - 1)
    colnames(locationTable) <- c("Location of Injury", "Number")
    locationTable
    
    #selected() %>% count(diag, wt = weight, sort = TRUE)
  }, width = "100%" )
  
  # calculates the rate for every 10,000 injuries, joins it to current table
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # output table - if else statement in two parts for rate/count
  output$age_sex <- renderPlot({
    # for COUNT - create the following plot
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        
        geom_line(linewidth = .5) +
        # sets line aesthetics
        
        labs(x = "Age", 
             y = "Estimated Number of Injuries") +
        # labels
        
        theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
              plot.subtitle = element_text(margin = margin(0,0,20,0), hjust = 0.5),
              axis.title.x = element_text(margin = margin(20,0,0,0)),
              axis.title.y = element_text(margin = margin(0,20,0,0)),
              # sets bold axis titles
              
              axis.line  = element_line(linewidth = 1, color = "black"),
              # bolder x and y axis
              
              panel.grid.major.y =element_line(color= "grey80", linewidth = .75),
              panel.grid.major.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank())
        # grids
        
        
    } else {
      summary() %>%
        # For RATE - creates this plot
        ggplot(aes(age, rate, colour = sex)) +
        
        geom_line(na.rm = TRUE, linewidth = .5) +
        # sets line aesthetics
        
        labs(x = "Age", 
             y = "Rate per 10,000 Injuries") +
        # labels
        
        theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
              plot.subtitle = element_text(margin = margin(0,0,20,0), hjust = 0.5),
              axis.title.x = element_text(margin = margin(20,0,0,0)),
              axis.title.y = element_text(margin = margin(0,20,0,0)),
              # sets bold axis titles
              
              axis.line  = element_line(linewidth = 1, color = "black"),
              # bolder x and y axis
              
              panel.grid.major.y =element_line(color= "grey80", linewidth = .75),
              panel.grid.major.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank())
      # grids
    }
  }, res = 96)

  
    output$age_sex <- renderPlot({
    # for COUNT - create the following plot
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        
        geom_line(linewidth = .5) +
        # sets line aesthetics
        
        labs(x = "Age", 
             y = "Estimated Number of Injuries") +
        # labels
        
        theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
              plot.subtitle = element_text(margin = margin(0,0,20,0), hjust = 0.5),
              axis.title.x = element_text(margin = margin(20,0,0,0)),
              axis.title.y = element_text(margin = margin(0,20,0,0)),
              # sets bold axis titles
              
              axis.line  = element_line(linewidth = 1, color = "black"),
              # bolder x and y axis
              
              panel.grid.major.y =element_line(color= "grey80", linewidth = .75),
              panel.grid.major.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank())
        # grids
        
        
    } else {
      summary() %>%
        # For RATE - creates this plot
        ggplot(aes(age, rate, colour = sex)) +
        
        geom_line(na.rm = TRUE, linewidth = .5) +
        # sets line aesthetics
        
        labs(x = "Age", 
             y = "Rate per 10,000 Injuries") +
        # labels
        
        theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
              plot.subtitle = element_text(margin = margin(0,0,20,0), hjust = 0.5),
              axis.title.x = element_text(margin = margin(20,0,0,0)),
              axis.title.y = element_text(margin = margin(0,20,0,0)),
              # sets bold axis titles
              
              axis.line  = element_line(linewidth = 1, color = "black"),
              # bolder x and y axis
              
              panel.grid.major.y =element_line(color= "grey80", linewidth = .75),
              panel.grid.major.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank())
      # grids
    }
  }, res = 96)

  # second plot - over the year
    
    overTime <- reactive({
      selected() %>%
        mutate(month = floor_date(trmt_date, "month")) %>%
        group_by(month, input$code) %>%
        summarise(n()) %>%
        rename(number = 'n()')
    })
    
    output$time <- renderPlot({
      
      overTime() %>%
        ggplot(aes(month, y =number)) +
        
        geom_line() +
        # sets line aesthetics
        
        labs(x = "Time", 
             y = "Monthly Number of Injuries") +
        # labels
        
        theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
              plot.subtitle = element_text(margin = margin(0,0,20,0), hjust = 0.5),
              axis.title.x = element_text(margin = margin(20,0,0,0)),
              axis.title.y = element_text(margin = margin(0,20,0,0)),
              # sets bold axis titles
              
              axis.line  = element_line(linewidth = 1, color = "black"),
              # bolder x and y axis
              
              panel.grid.major.y =element_line(color= "grey80", linewidth = .75),
              panel.grid.major.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.x =element_line(color = alpha("#d3d3d3", 0.5), 
                                               linetype = "dashed", 
                                               linewidth = .75),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank())
      # grids
      
    }, res = 96)
    
    
    
  
  # pulls a random narrative from the options for the selected product
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  
  output$narrative <- renderText(narrative_sample())
  
  # Selects a narrative
  selectedN <- reactive(selected() %>% pull(narrative))

  # scroll through narratives
  res <- reactiveValues(narrativeNo = 1)
  
  # for the next story
  observeEvent(input$nextStory, {
    # if the narrative number (1) is larger than the selected, loop back to one
    if(res$narrativeNo > length(selectedN())) res$narrativeNo <- 1
    # if not, display the narrative number plus 1
    else res$narrativeNo <- res$narrativeNo + 1
  })
  
  # for the previous story
  observeEvent(input$prevStory, {
    # if the narrative number is less than 2, then select the narrative number that is equal to the length of the selected
    if(res$narrativeNo < 2) res$narrativeNo <- length(selectedN())
    # if not, display the narrative number minus 1
    else res$narrativeNo <- res$narrativeNo - 1
  })
  
  # new texts
  output$narrativeMiddle <- renderText({
    # renders selected 
    allNarratives <- selectedN()
    # assigns the select to the output
    allNarratives[res$narrativeNo]
  })
}



shinyApp(ui, server)