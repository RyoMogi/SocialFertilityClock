# Return the requested dataset ----

sel_bc <- reactive({
  switch(input$BC_Fig2,
         "1940-1949" = "1940s",
         "1950-1959" = "1950s",
         "1960-1969" = "1960s"
         )
})

sel_ggsname <- reactive({
  switch(input$dataset_Fig2,
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
         "The UK" = "GBR", 
         "The US" = "USA"
  )
})

lx49_bc <- reactive({
  
  name_cntr <- c("BLR", "BEL", "BGR", "CZE", "EST", "FRA", 
                 "DEU", "HUN", "ITA", "LTU", "NLD", "NOR", 
                 "POL", "ROM", "RUS", "ESP", "SWE", "GBR", "USA")
  
  if(sel_bc() %in% c("1940s", "1950s")){
    name_cntr <- name_cntr[name_cntr != "USA"]
  }
  
  name_cntr_bc <- paste(name_cntr, sel_bc(), sep = "")
  
  lx49 <- c()
  
  for(i in 1:length(name_cntr_bc)){
    Mx <- readRDS(paste("Data/", name_cntr_bc[i],".Rda", sep = ""))
    c_lx49 <- Getlx49(Mx = Mx, union = "S")
    c_lx   <- Getlx(Mx = Mx, union = "S", MinAge = 15)
    c_lxSP0 <- c_lx[, 1]
    Age <- 15:49
    CountryBC <- rep(name_cntr_bc[i], length(Age))
    c_lx49 <- cbind(Age, CountryBC, c_lx49, c_lxSP0)
    
    lx49 <- rbind(lx49, c_lx49)
  }
  
  return(lx49)
})

output$Figure2_try <- renderPlotly({
  Sys.sleep(1)
  D_Fig2 <- lx49_bc() %>% 
    as.data.frame() %>% 
    mutate(Age = as.numeric(as.character(Age)),
           Proportion = as.numeric(as.character(c_lx49)),
           Proportion = round(Proportion, 2),
           c_lxSP0 = as.numeric(as.character(c_lxSP0)),
           c_lxSP0 = round(c_lxSP0, 2)) %>% 
    mutate(Country = case_when(str_detect(CountryBC, "HUN") ~ "Hungary",
                               str_detect(CountryBC, "NOR") ~ "Norway",
                               str_detect(CountryBC, "FRA") ~ "France",
                               str_detect(CountryBC, "BGR") ~ "Bulgaria",
                               str_detect(CountryBC, "BLR") ~ "Belarus",
                               str_detect(CountryBC, "ROM") ~ "Romania",
                               str_detect(CountryBC, "CZE") ~ "Czechia",
                               str_detect(CountryBC, "RUS") ~ "Russia",
                               str_detect(CountryBC, "SWE") ~ "Sweden",
                               str_detect(CountryBC, "EST") ~ "Estonia",
                               str_detect(CountryBC, "POL") ~ "Poland",
                               str_detect(CountryBC, "USA") ~ "The US",
                               str_detect(CountryBC, "LTU") ~ "Lithuania",
                               str_detect(CountryBC, "NLD") ~ "The Netherlands",
                               str_detect(CountryBC, "GBR") ~ "The UK",
                               str_detect(CountryBC, "DEU") ~ "Germany",
                               str_detect(CountryBC, "ITA") ~ "Italy",
                               str_detect(CountryBC, "BEL") ~ "Belgium",
                               str_detect(CountryBC, "ESP") ~ "Spain"))
  
  D_Fig2_unsel <- D_Fig2 %>% 
    filter(str_detect(CountryBC, sel_ggsname(), negate = T))
  
  D_Fig2_sel <- D_Fig2 %>% 
    filter(str_detect(CountryBC, sel_ggsname()))
  
  ## biological fertility clock
  #Age <- 25:59
  ster <- c(10, 11, 12, 14, 17, 20, 24, 29, 35, 42, 
            51, 64, 82, 105, 133, 166, 204, 249, 306, 401, 
            546, 685, 785, 855, 895, 919, 937, 952, 965, 976,
            985, 991, 996, 999, 1000)
  prop_ster <- ster * 0.001
  Proportion <- c(rep(0, 10), prop_ster[1:25])
  Age <- 15:49
  BFC <- data.frame(Age, Proportion)
  
  Fig2 <- D_Fig2_unsel %>% 
    ggplot(aes(x = Age, y = Proportion)) +
    geom_line(aes(group = Country), colour = "grey") +
    geom_line(data = D_Fig2_sel, aes(group = Country), 
              size = 2, colour = "#ff0303") +
    geom_line(data = BFC, size = 1.5, colour = "#238B45", linetype = "dashed") +
  #ylab(expression(paste("Proportion remaining single and childless (", S[0], ") by age 50, for women in ", S[0], " at each age"))) +
    theme_bw()
  
  ggplotly(Fig2, dynamicTicks = T) %>% 
    layout(
      xaxis = list(title = TeX("\\text{Age at which women were single and childless (} S_0 \\text{)}")),
      yaxis = list(
        title = TeX("\\text{Proportion remaining single and childless (} S_0 \\text{) by age 50}")
      ),
      font = list(size = 8.5)
    ) %>%
    config(modeBarButtonsToRemove = c("toImage", "zoom2d", "pan2d", "autoScale2d", 
                                      "hoverClosestCartesian", "hoverCompareCartesian"),
           mathjax = "cdn")
})



#output$Figure2 <- renderSvgPanZoom({
#  Sys.sleep(1)
#  Fig2 <- lx49_1960s() %>% 
#    as.data.frame() %>% 
#    mutate(Age = as.numeric(as.character(Age)),
#           c_lx49 = as.numeric(as.character(c_lx49)),
#           c_lxSP0 = as.numeric(as.character(c_lxSP0)),
#           c_lxSP0 = round(c_lxSP0, 2)) %>% 
#    mutate(Country = case_when(CountryBC == "HUN1960s" ~ 19,
#                               CountryBC == "NOR1960s" ~ 18,
#                               CountryBC == "FRA1960s" ~ 17,
#                               CountryBC == "BGR1960s" ~ 16,
#                               CountryBC == "BLR1960s" ~ 15,
#                               CountryBC == "ROM1960s" ~ 14,
#                               CountryBC == "CZE1960s" ~ 13,
#                               CountryBC == "RUS1960s" ~ 12,
#                               CountryBC == "SWE1960s" ~ 11,
#                               CountryBC == "EST1960s" ~ 10,
#                               CountryBC == "POL1960s" ~ 9,
#                               CountryBC == "USA1960s" ~ 8,
#                               CountryBC == "LTU1960s" ~ 7,
#                               CountryBC == "NLD1960s" ~ 6,
#                               CountryBC == "GBR1960s" ~ 5,
#                               CountryBC == "DEU1960s" ~ 4,
#                               CountryBC == "ITA1960s" ~ 3,
#                               CountryBC == "BEL1960s" ~ 2,
#                               CountryBC == "ESP1960s" ~ 1)) %>%
#    ggplot(aes(x = Age, y = c_lx49)) +
#    geom_line(aes(group = Country), colour = "grey") +
#    geom_line(data = . %>% filter(CountryBC == sel_ggsname1960s()), aes(x = Age, y = c_lx49), 
#              size = 2, colour = "orangered1") +
#    ylab(expression(paste("Proportion remaining single and childless (", S[0], ") by age 50, for women in ", S[0], " at each age"))) +
#    theme_bw()
#  
#  svgPanZoom(
#    svglite::stringSVG(print(Fig2),standalone=F),
#    controlIconsEnabled = T, viewBox = F
#    )
#  })

#output$title_Figure2 <- includeMarkdown("md/title_Fig2.Rmd")



