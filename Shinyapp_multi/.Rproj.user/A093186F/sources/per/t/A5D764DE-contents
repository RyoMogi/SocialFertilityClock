# Return the requested dataset ----
sel_ggsname_Fig3 <- reactive({
  switch(input$dataset_Fig3,
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

lx49 <- reactive({
  
  name_cntr <- c("BLR1940s", "BLR1950s", "BLR1960s", "BEL1940s", 
                 "BEL1950s", "BEL1960s", "BGR1940s", "BGR1950s", "BGR1960s", "CZE1940s", "CZE1950s", 
                 "CZE1960s", "EST1940s", "EST1950s", "EST1960s", "FRA1940s", "FRA1950s", "FRA1960s", 
                 "DEU1940s", "DEU1950s", "DEU1960s", "HUN1940s", "HUN1950s", "HUN1960s", "ITA1940s", 
                 "ITA1950s", "ITA1960s", "LTU1940s", "LTU1950s", "LTU1960s", "NLD1940s", "NLD1950s", 
                 "NLD1960s", "NOR1940s", "NOR1950s", "NOR1960s", "POL1940s", "POL1950s", "POL1960s", 
                 "ROM1940s", "ROM1950s", "ROM1960s", "RUS1940s", "RUS1950s", "RUS1960s", "ESP1940s", 
                 "ESP1950s", "ESP1960s", "SWE1940s", "SWE1950s", "SWE1960s", "GBR1940s", "GBR1950s", 
                 "GBR1960s", "USA1960s")
  
  lx49 <- c()
  
  for(i in 1:length(name_cntr)){
    Mx <- readRDS(paste("Data/", name_cntr[i],".Rda", sep = ""))
    c_lx49 <- Getlx49(Mx = Mx, union = "S")
    c_lx   <- Getlx(Mx = Mx, union = "S", MinAge = 15)
    c_lxSP0 <- c_lx[, 1]
    Age <- 15:49
    CountryBC <- rep(name_cntr[i], length(Age))
    c_lx49 <- cbind(Age, CountryBC, c_lx49, c_lxSP0)
    
    lx49 <- rbind(lx49, c_lx49)
  }
  
  return(lx49)
})

lx_x <- reactive({
  
  lx49() %>% 
    as.data.frame() %>% 
    mutate(Age = as.numeric(as.character(Age)),
           c_lx49 = as.numeric(as.character(c_lx49)),
           c_lxSP0 = as.numeric(as.character(c_lxSP0)),
           c_lxSP0 = round(c_lxSP0, 2)) %>%
    filter(Age == input$MinAge_Fig3) %>% 
    select(CountryBC, c_lxSP0)
  
})

Dlx49 <- reactive({
  
  lx49() %>% 
    as.data.frame() %>% 
    mutate(Age = as.numeric(as.character(Age)),
           c_lx49 = as.numeric(as.character(c_lx49)),
           c_lxSP0 = as.numeric(as.character(c_lxSP0)),
           bc = str_extract(CountryBC, "[0-9]{4}[s]"),
           c_lx49 = round(c_lx49, 2),
           c_lxSP0 = round(c_lxSP0, 2)) %>%
    select(-c_lxSP0) %>% 
    filter(Age %in% c(15, input$MinAge_Fig3)) %>% 
    mutate(Age = ifelse(Age == 15, "Age15", "Age_Min")) %>% 
    spread(key = Age, value = c_lx49) %>% 
    left_join(lx_x(), by = c("CountryBC")) %>% 
    mutate(c_lxSP0_100 = c_lxSP0 * 100)
})

Min_propMinAge50 <- reactive({
  round(min(Dlx49()$Age_Min), 2)
})

Max_propMinAge50 <- reactive({
  round(max(Dlx49()$Age_Min), 2)
})

Min_prop30 <- reactive({
  trunc(min(Dlx49()$c_lxSP0_100))
})

Max_prop30 <- reactive({
  ceiling(max(Dlx49()$c_lxSP0_100))
})

#output$Figure3 <- renderPlot({
#  Sys.sleep(1)
#  cp_bw <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
#  main_lx49 <- Dlx49() %>% 
#    ggplot(aes(x = Age15, y = Age_Min, colour = bc)) +
#    geom_point(aes(size = c_lxSP0_100)) +
#    scale_x_continuous(limits = c(0, 0.15), breaks = c(0, 0.05, 0.1, 0.15)) +
#    scale_y_continuous(limits = c(Min_propMinAge50(), Max_propMinAge50())) +
#    labs(x = expression(paste("Proportion in ", S[0], " by age 50")), y = expression(paste("Proportion in ", S[0]," at age 50 among women who remain in", S[0], " at the selected age"))) +
#    scale_color_manual(name = "Birth cohort", values = c(cp_bw[1], cp_bw[2], cp_bw[3])) +
#    scale_size(name = expression(paste("% of ", S[0], " at the selected age")), limits = c(Min_prop30(), Max_prop30())) +
#    geom_line(data = . %>% 
#                filter(str_detect(CountryBC, sel_ggsname_Fig3())), colour = 1)
#  
#  xdens <- axis_canvas(main_lx49, axis = "x") +
#    geom_density(data = Dlx49(), aes(x = Age15, fill = bc),
#                 alpha = 0.7, size = 0.2) +
#    scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))
#  
#  ydens <- axis_canvas(main_lx49, axis = "y", coord_flip = T) +
#    geom_density(data = Dlx49(), aes(x = Age_Min, fill = bc),
#                 alpha = 0.7, size = 0.2) +
#    coord_flip() +
#    scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))
#  
#  p1 <- insert_xaxis_grob(main_lx49, xdens, grid::unit(.2, "null"), position = "top")
#  p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
#  
#  ggdraw(p2)
#})

output$Figure3 <- renderPlot({
  Sys.sleep(1)
  cp_bw <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
  main_lx49 <- Dlx49() %>% 
    mutate(Proportion_S0 = c_lxSP0_100,
           Cohort = bc) %>% 
    ggplot(aes(x = Age15, y = Age_Min, colour = Cohort)) +
    geom_point(aes(size = Proportion_S0)) +
    scale_x_continuous(limits = c(0, 0.15), breaks = c(0, 0.05, 0.1, 0.15)) +
    scale_y_continuous(limits = c(0, Max_propMinAge50())) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    labs(x = expression(paste("Proportion in ", S[0], " by age 50")), 
         y = expression(paste("Proportion ", S[0]," at age 50 among women in ", S[0], " at the selected age", sep = ""))) +
    theme(text = element_text(size = 12)) +
    scale_color_manual(name = "Birth cohort", values = c(cp_bw[1], cp_bw[2], cp_bw[3])) +
    scale_size(limits = c(Min_prop30(), Max_prop30()),
               name = expression(paste("% of ", S[0], " at the selected age", sep = ""))) +
    geom_line(data = . %>% 
                filter(str_detect(CountryBC, sel_ggsname_Fig3())), colour = 1) +
    theme_bw()
  
  # This works well, but does not show two legends...
  #plotly_main <- ggplotly(main_lx49, dynamicTicks = T) %>% 
  #  layout(
  #    xaxis = list(
  #      title = TeX("\\text{Proportion in } S_0 \\text{ by age 50}")
  #      ),
  #    yaxis = list(
  #      title = TeX("\\text{Proportion in } S_0 \\text{ at age 50 among women who remain in } S_0 \\text{ at the selected age}")
  #    ),
  #    font = list(size = 9)
  #  ) %>%
  #  config(modeBarButtonsToRemove = c("toImage", "zoom2d", "pan2d", "select2d", "lasso2d", 
  #                                    "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
  #         mathjax = "cdn")
  
  # try to use ggplotly to draw marginal distribution; not work
  #subplot(
  #  ggplotly(geom_density(data = Dlx49(), aes(x = Age15, fill = bc),
  #                          alpha = 0.7, size = 0.2) + 
  #             scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))),
  #  plotly_empty(),
  #  ggplotly(main_lx49),
  #  ggplotly(geom_density(data = Dlx49(), aes(x = Age_Min, fill = bc),
  #                        alpha = 0.7, size = 0.2) + 
  #             scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))),
  #  nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
  #  shareX = TRUE, shareY = TRUE)
  #
  #subplot(
  #  plot_ly(data = Dlx49(), x = ~Age15)
  #)
  
  xdens <- axis_canvas(main_lx49, axis = "x") +
    geom_density(data = Dlx49(), aes(x = Age15, fill = bc),
                 alpha = 0.7, size = 0.2) +
    scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))
  
  ydens <- axis_canvas(main_lx49, axis = "y", coord_flip = T) +
    geom_density(data = Dlx49(), aes(x = Age_Min, fill = bc),
                 alpha = 0.7, size = 0.2) +
    coord_flip() +
    scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))
  
  p1 <- insert_xaxis_grob(main_lx49, xdens, grid::unit(.2, "null"), position = "top")
  p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
  
  ggdraw(p2)
})