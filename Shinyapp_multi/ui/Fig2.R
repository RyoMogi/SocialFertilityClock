## Fig 2

# Sidebar layout with input and output definitions
sidebarLayout(
  
  # Sidebar panel for inputs
  sidebarPanel(
    helpText("Choose a country that you want to calculate:"),
    
    # Input: Selector for choosing dataset
    selectInput(inputId = "BC_Fig2",
                label = "Birth cohort",
                choices = c("1940-1949", "1950-1959", "1960-1969"),
                selected = "1960-1969"),
    
    selectInput(inputId = "dataset_Fig2",
                label = "Country",
                choices = c("Belarus", "Belgium", "Bulgaria", "Czech Republic", 
                            "Estonia", "France", "Germany", "Hungary", "Italy", 
                            "Lithuania","Netherlands", "Norway", "Poland",
                            "Romania", "Russia", "Spain", "Sweden",
                            "The UK", "The US"),
                selected = "Sweden"),
    
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
    #svgPanZoomOutput(outputId = "Figure2", width = "100%", height = "500px") %>% withSpinner(color = "#fcb3e2"),
    plotlyOutput(outputId = "Figure2_try", width = "100%", height = "500px") %>% withSpinner(color = "#fcb3e2"),
    # Output: Figure title
    #textOutput(outputId = "title_Figure2")
    includeMarkdown("md/title_Fig2.Rmd")
  )
)