## Fig1

# Sidebar layout with input and output definitions
sidebarLayout(
  
  # Sidebar panel for inputs
  sidebarPanel(
    helpText("Choose a country that you want to calculate:"),
    
    # Input: Selector for choosing dataset
    selectInput(inputId = "dataset",
                label = "Country",
                choices = c("Belarus", "Belgium", "Bulgaria", "Czech Republic", 
                            "Estonia", "France", "Germany", "Hungary", "Italy", 
                            "Lithuania","Netherlands", "Norway", "Poland",
                            "Romania", "Russia", "Spain", "Sweden",
                            "The UK", "The US"),
                selected = "Sweden"),
    
    helpText("Choose the starting age:"),
    
    sliderInput(inputId = "MinAge", label = "Starting age",
                min = 15, max = 48, value = 15, step = 1, ticks = F),
    
    submitButton("Update Figures", icon("refresh")),
    
    br(),
    
    # Source
    "Source: ", tags$a(href = "https://www.ggp-i.org/", "The Hamonized Histories"),
    br(),
    # Note
    "Note: The US does not have data on people born in 1940s and 1950s."
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    
    ## Output: The transition rate of each parity
    plotOutput(outputId = "Plot_lx_S_15") %>% withSpinner(color = "#fcb3e2"),
    # Output: Figure title
    textOutput(outputId = "title_plot_S_15"), 
    tags$br(),
    
    ## Output: The transition rate of each parity
    plotOutput(outputId = "Plot_lx_S") %>% withSpinner(color = "#fcb3e2"),
    # Output: Figure title
    textOutput(outputId = "title_plot_S"),
    tags$br(),
    
    plotOutput(outputId = "Plot_lx_C"),
    # Output: Figure title
    textOutput(outputId = "title_plot_C"),
    tags$br(),
    
    plotOutput(outputId = "Plot_lx_M"),
    # Output: Figure title
    textOutput(outputId = "title_plot_M") 
    
  )
)