library(shiny)
library(markdown)
library(readstata13)
library(tidyverse)
library(ggpubr)
library(shinycssloaders)
library(highcharter)
library(magrittr)
library(cowplot)
#library(svgPanZoom)
#library(svglite)
library(plotly)
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

Getlx49 <- function(Mx, union){
  
  if(union == "S"){
    I <- diag(9) # diag: identity matrix
    radix = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
    lx_49 <- c()
    
    for(MinAge in 15:48){
      lx <- rbind(radix, matrix(rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 9))
      
      for(i in (MinAge - 14):34){
        px <- (I - (0.5 * Mx[[i]])) %*% solve(I + 0.5 * Mx[[i]]) # solve: inverse of ()
        
        k <- which(i == (MinAge - 14):34)
        lx[k + 1, ] <- lx[k, ] %*% px
      }
      
      lx_49 <- c(lx_49, lx[nrow(lx), 1])
    }
    
    lx_49 <- c(lx_49, 1)
    
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
  
  return(lx_49)
  
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

#### ---- Server ----
options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {
  for (file in list.files("server")) {
    source(file.path("server", file), local = T)
  }
})