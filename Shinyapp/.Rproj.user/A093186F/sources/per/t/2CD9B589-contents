# Return the requested dataset ----
sel_ggsname_Fig1 <- reactive({
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
  readRDS(paste("Data/", sel_ggsname_Fig1(),"1940s.Rda", sep = ""))
})

Mx_1950s <- reactive({
  readRDS(paste("Data/", sel_ggsname_Fig1(),"1950s.Rda", sep = ""))
})

Mx_1960s <- reactive({
  readRDS(paste("Data/", sel_ggsname_Fig1(),"1960s.Rda", sep = ""))
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
  if(sel_ggsname_Fig1() == "USA"){
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
  paste("Figure 1: The distribution of family statuses at subsequent ages for women who were single and childless at age 15 in ", input$dataset, " by birth cohorts, 1940s, 1950s, and 1960s", sep = "")
})

## Plot lx
output$Plot_lx_S <- renderPlot({
  
  if(sel_ggsname_Fig1() == "USA" & input$MinAge > 15){
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
  
  if(sel_ggsname_Fig1() == "USA" & input$MinAge > 15){
    paste("Figure 2: The distribution of family statuses at subsequent ages for women who were single and childless at age ", input$MinAge, " for the cohorts born in 1960s (", prop_SP0_1960s(), "%) in ", input$dataset, sep = "")
  } else {
    
    if(input$MinAge == 15){
      return()
    } else {
      paste("Figure 2: The distribution of family statuses at subsequent ages for women who were single and childless at age ", input$MinAge, " in ", input$dataset, " by birth cohorts, 1940s (", prop_SP0_1940s(), "%), 1950s (", prop_SP0_1950s(), "%), and 1960s (", prop_SP0_1960s(), "%)", sep = "")
    }
  }
})

output$Plot_lx_C <- renderPlot({
  if(sel_ggsname_Fig1() == "USA" & prop_CP0_1960s() > 0){
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
  if(sel_ggsname_Fig1() == "USA" & prop_CP0_1960s() > 0){
    paste("Figure 3: The distribution of family statuses at subsequent ages for women who were cohabiting and childless at age ", input$MinAge, " for the cohorts born in 1960s (", prop_CP0_1960s(), "%) in ", input$dataset, sep = "")
  } else {
    
    if(prop_CP0_1960s() == 0){
      return()
    } else {
      paste("Figure 3: The distribution of family statuses at subsequent ages for women who were cohabiting and childless at age ", input$MinAge, " in ", input$dataset, " by birth cohorts, 1940s (", prop_CP0_1940s(), "%), 1950s (", prop_CP0_1950s(), "%), and 1960s (", prop_CP0_1960s(), "%)", sep = "")
    }
  }
})

output$Plot_lx_M <- renderPlot({
  if(sel_ggsname_Fig1() == "USA" & prop_MP0_1960s() > 0){
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
  if(sel_ggsname_Fig1() == "USA" & prop_MP0_1960s() > 0){
    paste("Figure 4: The distribution of family statuses at subsequent ages for women who were ever-married and childless at age ", input$MinAge, " for the cohorts born in 1960s (", prop_MP0_1960s(), "%) in ", input$dataset, sep = "")
  } else {
    
    if(prop_MP0_1960s() == 0){
      return()
    } else {
      paste("Figure 4: The distribution of family statuses at subsequent ages for women who were ever-married and childless at age ", input$MinAge, " in ", input$dataset, " by birth cohorts, 1940s (", prop_MP0_1940s(), "%), 1950s (", prop_MP0_1950s(), "%), and 1960s (", prop_MP0_1960s(), "%)", sep = "")
    }
  }
})