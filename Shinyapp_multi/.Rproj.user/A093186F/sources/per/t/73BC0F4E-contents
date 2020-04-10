## Fig 3

# Sidebar layout with input and output definitions
sidebarLayout(
  
  # Sidebar panel for inputs
  sidebarPanel(
    helpText("Choose a country that you want to calculate:"),
    
    # Input: Selector for choosing dataset
    selectInput(inputId = "dataset_Fig3",
                label = "Country",
                choices = c("Belarus", "Belgium", "Bulgaria", "Czech Republic", 
                            "Estonia", "France", "Germany", "Hungary", "Italy", 
                            "Lithuania","Netherlands", "Norway", "Poland",
                            "Romania", "Russia", "Spain", "Sweden",
                            "The UK", "The US"),
                selected = "Sweden"),
    
    helpText("Choose the starting age:"),
    
    sliderInput(inputId = "MinAge_Fig3", label = "Starting age",
                min = 16, max = 48, value = 30, step = 1, ticks = F),
    
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
    plotOutput(outputId = "Figure3", width = "100%", height = "500px") %>% withSpinner(color = "#fcb3e2"),
    # Output: Figure title
    #textOutput(outputId = "title_Figure3")
    includeMarkdown("md/title_Fig3.Rmd")
  )
)