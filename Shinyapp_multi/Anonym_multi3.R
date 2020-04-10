library(shiny)
library(markdown)
library(readstata13)
library(tidyverse)
library(ggpubr)
library(shinycssloaders)
library(highcharter)
library(magrittr)
`%out%` = Negate(`%in%`)

#### ---- Function ----
cb2 <- c("#d7191c", "#fdae61", "#1a9641", "#abd9e9", "#2c7bb6")
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

Getlx <- function(Mx, union, MinAge){
  if(union == "S"){
    I <- diag(9) # diag: identity matrix
    radix = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
    lx <- rbind(radix, matrix(rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 9))
    
    for(i in (MinAge - 14):34){
      px <- (I - (0.5 * Mx[[i]])) %*% solve(I + 0.5 * Mx[[i]]) # solve: inverse of ()
      
      k <- which(i == (MinAge - 14):34)
      lx[k + 1, ] <- lx[k, ] %*% px
    }
    
    colnames(lx) <- c("SP0", "CP0", "MP0", "SP1", "CP1", "MP1", "SP2", "CP2", "MP2")
    
  } else {
    
    if(union == "C"){
      I <- diag(6) # diag: identity matrix
      radix = c(1, 0, 0, 0, 0, 0)
      lx <- rbind(radix, matrix(rep(c(0, 0, 0, 0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 6))
      
      for(i in (MinAge - 14):34){
        px <- (I - (0.5 * Mx[[i]][-c(1, 4, 7), -c(1, 4, 7)])) %*% solve(I + 0.5 * Mx[[i]][-c(1, 4, 7), -c(1, 4, 7)]) # solve: inverse of ()
        
        k <- which(i == (MinAge - 14):34)
        lx[k + 1, ] <- lx[k, ] %*% px
      }
      
      colnames(lx) <- c("CP0", "MP0", "CP1", "MP1", "CP2", "MP2")
      
    } else {
      
      I <- diag(3) # diag: identity matrix
      radix = c(1, 0, 0)
      lx <- rbind(radix, matrix(rep(c(0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 3))
      
      for(i in (MinAge - 14):34){
        px <- (I - (0.5 * Mx[[i]][c(3, 6, 9), c(3, 6, 9)])) %*% solve(I + 0.5 * Mx[[i]][c(3, 6, 9), c(3, 6, 9)]) # solve: inverse of ()
        
        k <- which(i == (MinAge - 14):34)
        lx[k + 1, ] <- lx[k, ] %*% px
      }
      
      colnames(lx) <- c("MP0", "MP1", "MP2")
      
    }
  }
  
  return(lx)
}

Plotlx <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  
  if(is.null(lx1)){
    Plot1 <- NULL
    
  } else {
    Plot1 <- lx1 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge1:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1940s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                                   "#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                                   "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx2)){
    Plot2 <- NULL
    
  } else {
    Plot2 <- lx2 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge2:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1950s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                                   "#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                                   "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx3)){
    Plot3 <- NULL
    
  } else {
    Plot3 <- lx3 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge3:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1960s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                                   "#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                                   "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion in each union and parity status", rot = 90))
  
  return(Allplot)
}

Plotlx_C <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  if(is.null(lx1)){
    Plot1 <- NULL
    
  } else {
    
    Plot1 <- lx1 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge1:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1940s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx2)){
    Plot2 <- NULL
    
  } else {
    
    Plot2 <- lx2 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge2:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1950s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx3)){
    Plot3 <- NULL
    
  } else {
    
    Plot3 <- lx3 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge3:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1960s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                   "#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                   "Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion in each union and parity status", rot = 90))
  
  return(Allplot)
}

Plotlx_M <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  if(is.null(lx1)){
    Plot1 <- NULL
    
  } else {
    
    Plot1 <- lx1 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge1:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1940s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx2)){
    Plot2 <- NULL
    
  } else {
    Plot2 <- lx2 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge2:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1950s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  if(is.null(lx3)){
    Plot3 <- NULL
    
  } else {
    
    Plot3 <- lx3 %>% 
      as.data.frame() %>% 
      mutate(Age = MinAge3:49) %>%
      gather(key = UP, value = Number, -Age) %>%
      mutate(Number = ifelse(Number < 0, 0, Number),
             UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>% 
      group_by(Age) %>% 
      mutate(Number = Number %>% prop.table %>% multiply_by(1)) %>%
      ggplot(aes(x = Age, y = Number, fill = UP)) +
      geom_area(position = position_stack(reverse = T), stat = "identity") +
      labs(x = "Age", y = "", title = "1960s") +
      guides(fill = guide_legend(nrow = 2)) +
      scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                        labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                        name = "") +
      theme(legend.position = "bottom") +
      xlim(15, 49)
  }
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion in each union and parity status", rot = 90))
  
  return(Allplot)
}

#### ---- UI ----
ui <- fluidPage(
  
  # App title
  titlePanel("Social Fertility Clock"),
  
  #fluidRow(column(4,
  #                tags$strong("Share this: "),
  #                
  #                # Twitter sharing link
  #                tags$a(href = "http://twitter.com/share?url=https://ryom.shinyapps.io/Shiny_CALC/&text=Shinyapp of Cross Average length of Life Childless (CALC) by @rmogimogi", 
  #                       target = "_blank", 
  #                       tags$img(height = "30px", 
  #                                src = "twitter.png")),
  #                
  #                # Facebook Sharing link
  #                tags$a(href = "http://www.facebook.com/sharer.php?u=https://ryom.shinyapps.io/Shiny_CALC/", 
  #                       target = "_blank", 
  #                       tags$img(height = "30px", 
  #                                src = "facebook.png")),
  #                # Email Sharing link
  #                tags$a(href = "mailto:?Subject=Shinyapp of Cross Average length of Life Childless&Body=This is a supplemental material of the article 'Cross Average length of Life Childless' written by Mogi, R. and Canudas-Romo, V. https://ryom.shinyapps.io/Shiny_CALC/", 
  #                       tags$img(height = "30px", src = "email.png"))
  #)),
  
  tags$br(),
  
  # Explanation of this app
  #tags$p("This is a supplemental material of the article 'Social fertility clock'"),
  #tags$p("This interactive tool presents graphs and table showing the results of multistate."),
  
  tags$br(),
  
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
      "Source: ", tags$a(href = "https://www.ggp-i.org/", "The Hamonized Histories")
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
)

#### ---- Server ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  sel_ggsname <- reactive({
    switch(input$dataset,
           #"Austria" = "AUT", 
           "Belarus" = "BLR", 
           "Belgium" = "BEL", 
           "Bulgaria" = "BGR", 
           "Czech Republic" = "CZE", 
           "Estonia" = "EST", 
           "France" = "FRA", 
           "Germany" = "DEU", 
           "Hungary" = "HUN", 
           "Italy" = "ITA", 
           "Lithuania" = "LTU",
           "Netherlands" = "NLD", 
           "Norway" = "NOR", 
           "Poland" = "POL",
           "Romania" = "ROM", 
           "Russia" = "RUS", 
           "Spain" = "ESP", 
           "Sweden" = "SWE",
           "The UK" = "UK", 
           "The US" = "USA"
    )
  })
  
  ### Mx
  Mx_1940s <- reactive({
    readRDS(paste("Data/", sel_ggsname(),"1940s.Rda", sep = ""))
  })
  
  Mx_1950s <- reactive({
    readRDS(paste("Data/", sel_ggsname(),"1950s.Rda", sep = ""))
  })
  
  Mx_1960s <- reactive({
    readRDS(paste("Data/", sel_ggsname(),"1960s.Rda", sep = ""))
  })
  
  ### lx for single at age 15
  lx_S_15_1940s <- reactive({
    Getlx(Mx = Mx_1940s(), union = "S", MinAge = 15)
  })
  
  lx_S_15_1950s <- reactive({
    Getlx(Mx = Mx_1950s(), union = "S", MinAge = 15)
  })
  
  lx_S_15_1960s <- reactive({
    Getlx(Mx = Mx_1960s(), union = "S", MinAge = 15)
  })
  
  ### % of single and parity 0 by age
  prop_SP0_1940s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1940s()[age, 1]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_SP0_1950s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1950s()[age, 1]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_SP0_1960s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1960s()[age, 1]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  ### lx for single
  lx_S_1940s <- reactive({
    Getlx(Mx = Mx_1940s(), union = "S", MinAge = input$MinAge)
  })
  
  lx_S_1950s <- reactive({
    Getlx(Mx = Mx_1950s(), union = "S", MinAge = input$MinAge)
  })
  
  lx_S_1960s <- reactive({
    Getlx(Mx = Mx_1960s(), union = "S", MinAge = input$MinAge)
  })
  
  ### lx for cohabitation
  lx_C_1940s <- reactive({
    Getlx(Mx = Mx_1940s(), union = "C", MinAge = input$MinAge)
  })
  
  lx_C_1950s <- reactive({
    Getlx(Mx = Mx_1950s(), union = "C", MinAge = input$MinAge)
  })
  
  lx_C_1960s <- reactive({
    Getlx(Mx = Mx_1960s(), union = "C", MinAge = input$MinAge)
  })
  
  ### % of cohabitation and parity 0 by age
  prop_CP0_1940s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1940s()[age, 2]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_CP0_1950s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1950s()[age, 2]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_CP0_1960s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1960s()[age, 2]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  ### lx for married
  lx_M_1940s <- reactive({
    Getlx(Mx = Mx_1940s(), union = "M", MinAge = input$MinAge)
  })
  
  lx_M_1950s <- reactive({
    Getlx(Mx = Mx_1950s(), union = "M", MinAge = input$MinAge)
  })
  
  lx_M_1960s <- reactive({
    Getlx(Mx = Mx_1960s(), union = "M", MinAge = input$MinAge)
  })
  
  ### % of cohabitation and parity 0 by age
  prop_MP0_1940s <- reactive({
    
    age <- input$MinAge - 14
    prop <- NULL
    prop <- lx_S_15_1940s()[age, 3]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_MP0_1950s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1950s()[age, 3]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  prop_MP0_1960s <- reactive({
    
    age <- input$MinAge - 14
    prop <- lx_S_15_1960s()[age, 3]
    prop <- round(prop * 100, 1)
    return(prop)
  })
  
  
  ### ---- Results and plot ----
  ### Plot lx at age 15
  output$Plot_lx_S_15 <- renderPlot({
    if(sel_ggsname() == "USA"){
      Sys.sleep(1)
      Plotlx(lx1 = NULL, MinAge1 = NULL,
             lx2 = NULL, MinAge2 = NULL,
             lx3 = lx_S_15_1960s(), MinAge3 = 15)
    } else {
      Sys.sleep(1)
      Plotlx(lx1 = lx_S_15_1940s(), MinAge1 = 15,
             lx2 = lx_S_15_1950s(), MinAge2 = 15,
             lx3 = lx_S_15_1960s(), MinAge3 = 15)
    }
  })
  
  # Figure title of lx at age 15
  output$title_plot_S_15 <- renderText({
    paste("Figure 1: The proportion of women in each family status by age for single women in parity 0 at age 15 for the cohorts born in 1940s, 1950s, and 1960s in ", input$dataset, sep = "")
  })
  
  ## Plot lx
  output$Plot_lx_S <- renderPlot({
    
    if(sel_ggsname() == "USA" & input$MinAge > 15){
      Sys.sleep(1)
      Plotlx(lx1 = NULL, MinAge1 = NULL,
             lx2 = NULL, MinAge2 = NULL,
             lx3 = lx_S_1960s(), MinAge3 = input$MinAge)
    } else {
      
      if(input$MinAge == 15){
        return()
      } else {
        Sys.sleep(1)
        Plotlx(lx1 = lx_S_1940s(), MinAge1 = input$MinAge,
               lx2 = lx_S_1950s(), MinAge2 = input$MinAge,
               lx3 = lx_S_1960s(), MinAge3 = input$MinAge)
      }
    }
  })
  
  # Figure title of lx
  output$title_plot_S <- renderText({
    
    if(sel_ggsname() == "USA" & input$MinAge > 15){
      paste("Figure 2: The proportion of women in each family status by age for single women in parity 0 at age ", input$MinAge, " for the cohorts born in 1960s (", prop_SP0_1960s(), "%) in ", input$dataset, sep = "")
    } else {
      
      if(input$MinAge == 15){
        return()
      } else {
        paste("Figure 2: The proportion of women in each family status by age for single women in parity 0 at age ", input$MinAge, " for the cohorts born in 1940s (", prop_SP0_1940s(), "%), 1950s (", prop_SP0_1950s(), "%), and 1960s (", prop_SP0_1960s(), "%) in ", input$dataset, sep = "")
      }
    }
  })
  
  output$Plot_lx_C <- renderPlot({
    if(sel_ggsname() == "USA" & prop_CP0_1960s() > 0){
      Plotlx_C(lx1 = NULL, MinAge1 = NULL,
               lx2 = NULL, MinAge2 = NULL,
               lx3 = lx_C_1960s(), MinAge3 = input$MinAge)
    } else {
      
      if(prop_CP0_1960s() == 0){
        return()
      } else {
        Plotlx_C(lx1 = lx_C_1940s(), MinAge1 = input$MinAge,
                 lx2 = lx_C_1950s(), MinAge2 = input$MinAge,
                 lx3 = lx_C_1960s(), MinAge3 = input$MinAge)
      }
    }
  })
  
  # Figure title of lx
  output$title_plot_C <- renderText({
    if(sel_ggsname() == "USA" & prop_CP0_1960s() > 0){
      paste("Figure 3: The proportion of women in each family status by age for cohabiting women in parity 0 at age ", input$MinAge, " for the cohorts born in 1960s (", prop_CP0_1960s(), "%) in ", input$dataset, sep = "")
    } else {
      
      if(prop_CP0_1960s() == 0){
        return()
      } else {
        paste("Figure 3: The proportion of women in each family status by age for cohabiting women in parity 0 at age ", input$MinAge, " for the cohorts born in 1940s (", prop_CP0_1940s(), "%), 1950s (", prop_CP0_1950s(), "%), and 1960s (", prop_CP0_1960s(), "%) in ", input$dataset, sep = "")
      }
    }
  })
  
  output$Plot_lx_M <- renderPlot({
    if(sel_ggsname() == "USA" & prop_MP0_1960s() > 0){
      Plotlx_M(lx1 = NULL, MinAge1 = NULL,
               lx2 = NULL, MinAge2 = NULL,
               lx3 = lx_M_1960s(), MinAge3 = input$MinAge)
    } else {
      
      if(prop_MP0_1960s() == 0){
        return()
      } else {
        Plotlx_M(lx1 = lx_M_1940s(), MinAge1 = input$MinAge,
                 lx2 = lx_M_1950s(), MinAge2 = input$MinAge,
                 lx3 = lx_M_1960s(), MinAge3 = input$MinAge)
      }
    }
  })
  # Figure title of lx
  output$title_plot_M <- renderText({
    if(sel_ggsname() == "USA" & prop_MP0_1960s() > 0){
      paste("Figure 4: The proportion of women in each family status by age for married women in parity 0 at age ", input$MinAge, " for the cohorts born in 1960s (", prop_MP0_1960s(), "%) in ", input$dataset, sep = "")
    } else {
      
      if(prop_MP0_1960s() == 0){
        return()
      } else {
        paste("Figure 4: The proportion of women in each family status by age for married women in parity 0 at age ", input$MinAge, " for the cohorts born in 1940s (", prop_MP0_1940s(), "%), 1950s (", prop_MP0_1950s(), "%), and 1960s (", prop_MP0_1960s(), "%) in ", input$dataset, sep = "")
      }
    }
  })
}

shinyApp(ui = ui, server = server)