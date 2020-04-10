library(shiny)
library(markdown)
library(readstata13)
library(tidyverse)
library(ggpubr)
`%out%` = Negate(`%in%`)

#### ---- Function ----
cb2 <- c("#d7191c", "#fdae61", "#1a9641", "#abd9e9", "#2c7bb6")
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

Getlx <- function(cntr, MinAge, BC, union){
  for(i in 15:50){
    state_C1 <- paste("cntr$state_C1_", i, " <- ifelse(cntr$sage_cohab1 <=", i, "  &", i, " <= cntr$eage_cohab1, 1, 0)", sep = "")
    state_M1 <- paste("cntr$state_M1_", i, " <- ifelse(cntr$sage_mar1 <=", i, ", 1, 0)", sep = "")
    state_P1 <- paste("cntr$state_P1_", i, " <- ifelse(cntr$age_child1 <=", i, ", 1, 0)", sep = "")
    state_P2 <- paste("cntr$state_P2_", i, " <- ifelse(cntr$age_child2 <=", i, ", 1, 0)", sep = "")
    C1 <- paste("cntr$C1_", i, " <- ifelse(cntr$sage_cohab1 ==", i, ", 1, 0)", sep = "")
    M1 <- paste("cntr$M1_", i, " <- ifelse(cntr$sage_mar1 ==", i, ", 1, 0)", sep = "")
    P1 <- paste("cntr$P1_", i, " <- ifelse(cntr$age_child1 ==", i, " &", i, " < cntr$age_child2, 1, 0)", sep = "")
    P2 <- paste("cntr$P2_", i, " <- ifelse(cntr$age_child2 ==", i, ", 1, 0)", sep = "")
    #  P3 <- paste("cntr$P3_", i, " <- ifelse(", i, ">= cntr$age_child3, 1, 0)", sep = "")
    eval(parse(text = state_C1))
    eval(parse(text = state_M1))
    eval(parse(text = state_P1))
    eval(parse(text = state_P2))
    eval(parse(text = C1))
    eval(parse(text = M1))
    eval(parse(text = P1))
    eval(parse(text = P2))
    #  eval(parse(text = P3))
    
    SP0 <- paste("cntr$S1P0_", i, " <- ifelse(cntr$state_C1_", i, "== 0 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 0, 1, 0)", sep = "")
    SP1 <- paste("cntr$S1P1_", i, " <- ifelse(cntr$state_C1_", i, "== 0 & cntr$state_M1_", i, "== 0 & cntr$P1_", i, "== 1, cntr$order, 0)", sep = "")
    SP2 <- paste("cntr$S1P2_", i, " <- ifelse(cntr$state_C1_", i, "== 0 & cntr$state_M1_", i, "== 0 & cntr$P2_", i, "== 1, cntr$order, 0)", sep = "")
    #  SP3 <- paste("cntr$SP3_", i, " <- ifelse(cntr$C1_", i, "== 0 & cntr$M1_", i, "== 0 & cntr$P3_", i, "== 1, 1, 0)", sep = "")
    C1P0 <- paste("cntr$C1P0_", i, " <- ifelse(cntr$C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 0, cntr$order, 0)", sep = "")
    C1P1 <- paste("cntr$C1P1_", i, " <- ifelse((cntr$C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 1) | (cntr$state_C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$P1_", i, "== 1), cntr$order, 0)", sep = "")
    C1P2 <- paste("cntr$C1P2_", i, " <- ifelse((cntr$C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P2_", i, "== 1) | (cntr$state_C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$P2_", i, "== 1), cntr$order, 0)", sep = "")
    #  C1P3 <- paste("cntr$C1P3_", i, " <- ifelse(cntr$C1_", i, "== 1 & cntr$M1_", i, "== 0 & cntr$P3_", i, "== 1, cntr$order, 0)", sep = "")
    M1P0 <- paste("cntr$M1P0_", i, " <- ifelse(cntr$M1_", i, "== 1 & cntr$state_P1_", i, "== 0, cntr$order, 0)", sep = "")
    M1P1 <- paste("cntr$M1P1_", i, " <- ifelse((cntr$M1_", i, "== 1 & cntr$state_P1_", i, "== 1) | (cntr$state_M1_", i, "== 1 & cntr$P1_", i, "== 1), cntr$order, 0)", sep = "")
    M1P2 <- paste("cntr$M1P2_", i, " <- ifelse((cntr$M1_", i, "== 1 & cntr$state_P2_", i, "== 1) | (cntr$state_M1_", i, "== 1 & cntr$P2_", i, "== 1), cntr$order, 0)", sep = "")
    #  M1P3 <- paste("cntr$M1P3_", i, " <- ifelse(cntr$C1_", i, "== 0 & cntr$M1_", i, "== 1 & cntr$P3_", i, "== 1, cntr$order, 0)", sep = "")
    S1P1_exp <- paste("cntr$exp_S1P1_", i, " <- ifelse(cntr$state_C1_", i, "== 0 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 1 & cntr$state_P2_", i, "== 0, 1, 0)", sep = "")
    S1P2_exp <- paste("cntr$exp_S1P2_", i, " <- ifelse(cntr$state_C1_", i, "== 0 & cntr$state_M1_", i, "== 0 & cntr$state_P2_", i, "== 1, 1, 0)", sep = "")
    C1P0_exp <- paste("cntr$exp_C1P0_", i, " <- ifelse(cntr$state_C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 0, 1, 0)", sep = "")
    C1P1_exp <- paste("cntr$exp_C1P1_", i, " <- ifelse(cntr$state_C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P1_", i, "== 1, 1, 0)", sep = "")
    C1P2_exp <- paste("cntr$exp_C1P2_", i, " <- ifelse(cntr$state_C1_", i, "== 1 & cntr$state_M1_", i, "== 0 & cntr$state_P2_", i, "== 1, 1, 0)", sep = "")
    #  C1P3 <- paste("cntr$C1P3_", i, " <- ifelse(cntr$C1_", i, "== 1 & cntr$M1_", i, "== 0 & cntr$P3_", i, "== 1, cntr$order, 0)", sep = "")
    M1P0_exp <- paste("cntr$exp_M1P0_", i, " <- ifelse(cntr$state_M1_", i, "== 1 & cntr$state_P1_", i, "== 0, 1, 0)", sep = "")
    M1P1_exp <- paste("cntr$exp_M1P1_", i, " <- ifelse(cntr$state_M1_", i, "== 1 & cntr$state_P1_", i, "== 1, 1, 0)", sep = "")
    M1P2_exp <- paste("cntr$exp_M1P2_", i, " <- ifelse(cntr$state_M1_", i, "== 1 & cntr$state_P2_", i, "== 1, 1, 0)", sep = "")
    #  M1P3 <- paste("cntr$M1P3_", i, " <- ifelse(cntr$C1_", i, "== 0 & cntr$M1_", i, "== 1 & cntr$P3_", i, "== 1, cntr$order, 0)", sep = "")
    eval(parse(text = SP0))
    eval(parse(text = SP1))
    eval(parse(text = SP2))
    #  eval(parse(text = SP3))
    eval(parse(text = C1P0))
    eval(parse(text = C1P1))
    eval(parse(text = C1P2))
    #  eval(parse(text = C1P3))
    eval(parse(text = M1P0))
    eval(parse(text = M1P1))
    eval(parse(text = M1P2))
    #  eval(parse(text = M1P3))
    eval(parse(text = S1P1_exp))
    eval(parse(text = S1P2_exp))
    eval(parse(text = C1P0_exp))
    eval(parse(text = C1P1_exp))
    eval(parse(text = C1P2_exp))
    #  eval(parse(text = C1P3))
    eval(parse(text = M1P0_exp))
    eval(parse(text = M1P1_exp))
    eval(parse(text = M1P2_exp))
    #  eval(parse(text = M1P3))
  }
  
  # Exposure
  status <- c("S1P0", "exp_S1P1", "exp_S1P2", "exp_C1P0", "exp_C1P1", "exp_C1P2", "exp_M1P0", "exp_M1P1", "exp_M1P2")

  cntr_BC <- cntr %>% 
    filter(bc == BC)
  
  exposure <- c()
    for(i in 1:length(status)){
      D <- cntr_BC %>% 
        select(RESPID, bc, contains(status[i])) %>% 
        group_by(bc) %>% 
        summarise_all(funs(sum)) %>%
        select(-RESPID) %>% 
        gather(key = status, value = value, contains(status[i])) %>% 
        separate(col = status, c("status", "age"), sep = "_") 
        
      
        expo <- rep(NA, nrow(D) - 1)
      
      for(k in 1:nrow(D) - 1){
        expo[k] <- (D$value[k] + D$value[k+1]) / 2
      }
        
        exposure <- cbind(exposure, expo)
      
    }
    
    colnames(exposure) <- c("S1P0", "S1P1", "S1P2", "C1P0", "C1P1", "C1P2", "M1P0", "M1P1", "M1P2")
  
  # Event
    D <- cntr_BC %>% 
      select(contains("S1P"), -contains("exp"), -contains("S1P0")) %>% 
      gather(key = status, value = S1P, contains("S1P")) %>% 
      separate(col = status, c("status", "age"), sep = "_") %>%
      mutate(S1Pevent = case_when(S1P == 0 ~ "0",
                                  status == "S1P1" & S1P != 0 ~ str_extract(S1P, "....\\-S1P1"),
                                  status == "S1P2" & S1P != 0 ~ str_extract(S1P, "....\\-S1P2")))
    
    S1P_event <- table(D$S1Pevent, D$age)
    
    D <- cntr_BC %>% 
      select(contains("C1P"), -contains("exp")) %>% 
      gather(key = status, value = C1P, contains("C1P")) %>% 
      separate(col = status, c("status", "age"), sep = "_") %>%
      mutate(C1Pevent = case_when(C1P == 0 ~ "0",
                                  status == "C1P0" & C1P != 0 ~ "S1P0-C1P0",
                                  status == "C1P1" & C1P != 0 ~ str_extract(C1P, "....\\-C1P1"),
                                  status == "C1P2" & C1P != 0 ~ str_extract(C1P, "....\\-C1P2")))
    
    C1P_event <- table(D$C1Pevent, D$age)
    
    D <- cntr_BC %>% 
      select(contains("M1P"), -contains("exp")) %>% 
      gather(key = status, value = M1P, contains("M1P")) %>% 
      separate(col = status, c("status", "age"), sep = "_") %>%
      mutate(M1Pevent = case_when(M1P == 0 ~ "0",
                                  status == "M1P0" & M1P != 0 ~ str_extract(M1P, "....\\-M1P0"),
                                  status == "M1P1" & M1P != 0 ~ str_extract(M1P, "....\\-M1P1"),
                                  status == "M1P2" & M1P != 0 ~ str_extract(M1P, "....\\-M1P2")))
    
    M1P_event <- table(D$M1Pevent, D$age)
    
    event <- rbind(S1P_event, C1P_event, M1P_event)
  
  #### ---- Transition rate ----
  
  mij <- function(i, j){ # i: original state, j: transited state

    transit <- paste(i, j, sep = "-")

      alltransit <- rownames(event)
      
      if(j == "S1P0"){
        out <- exposure[, j] / exposure[, i]
        
      } else {
        
        if(transit %in% alltransit){
          out <- event[transit, -36] / exposure[, i]
          
        } else{
          out <- rep(0, 35)
          
        }
      }
    
    return(out)
  }
  
  m_S1P0C1P0 <- mij(i = "S1P0", j = "C1P0")
  m_S1P0M1P0 <- mij(i = "S1P0", j = "M1P0")
  m_S1P0S1P1 <- mij(i = "S1P0", j = "S1P1")
  m_S1P0C1P1 <- mij(i = "S1P0", j = "C1P1")
  m_S1P0M1P1 <- mij(i = "S1P0", j = "M1P1")
  m_C1P0M1P0 <- mij(i = "C1P0", j = "M1P0")
  m_C1P0C1P1 <- mij(i = "C1P0", j = "C1P1")
  m_C1P0M1P1 <- mij(i = "C1P0", j = "M1P1")
  m_M1P0M1P1 <- mij(i = "M1P0", j = "M1P1")
  
  m_S1P1C1P1 <- mij(i = "S1P1", j = "C1P1")
  m_S1P1M1P1 <- mij(i = "S1P1", j = "M1P1")
  m_S1P1S1P2 <- mij(i = "S1P1", j = "S1P2")
  m_S1P1C1P2 <- mij(i = "S1P1", j = "C1P2")
  m_S1P1M1P2 <- mij(i = "S1P1", j = "M1P2")
  m_C1P1M1P1 <- mij(i = "C1P1", j = "M1P1")
  m_C1P1C1P2 <- mij(i = "C1P1", j = "C1P2")
  m_C1P1M1P2 <- mij(i = "C1P1", j = "M1P2")
  m_M1P1M1P2 <- mij(i = "M1P1", j = "M1P2")
  
  m_S1P2C1P2 <- mij(i = "S1P2", j = "C1P2")
  m_S1P2M1P2 <- mij(i = "S1P2", j = "M1P2")
  m_C1P2M1P2 <- mij(i = "C1P2", j = "M1P2")
  
  mat_Mij_BC <- list()
  Mij <- matrix(NA, nrow = 9, ncol = 9)
    
  for(age in 1:length(m_S1P0C1P0)){
      Mij[1, 1] <- m_S1P0C1P0[age] + m_S1P0M1P0[age] + m_S1P0S1P1[age] + m_S1P0C1P1[age] + m_S1P0M1P1[age]
      Mij[1, 2] <- - m_S1P0C1P0[age]
      Mij[1, 3] <- - m_S1P0M1P0[age]
      Mij[1, 4] <- - m_S1P0S1P1[age]
      Mij[1, 5] <- - m_S1P0C1P1[age]
      Mij[1, 6] <- - m_S1P0M1P1[age]
      Mij[1, 7] <- 0
      Mij[1, 8] <- 0
      Mij[1, 9] <- 0
      Mij[2, 1] <- 0
      Mij[2, 2] <- m_C1P0M1P0[age] + m_C1P0C1P1[age] + m_C1P0M1P1[age]
      Mij[2, 3] <- - m_C1P0M1P0[age]
      Mij[2, 4] <- 0
      Mij[2, 5] <- - m_C1P0C1P1[age]
      Mij[2, 6] <- - m_C1P0M1P1[age]
      Mij[2, 7] <- 0
      Mij[2, 8] <- 0
      Mij[2, 9] <- 0
      Mij[3, 1] <- 0
      Mij[3, 2] <- 0
      Mij[3, 3] <- m_M1P0M1P1[age]
      Mij[3, 4] <- 0
      Mij[3, 5] <- 0
      Mij[3, 6] <- - m_M1P0M1P1[age]
      Mij[3, 7] <- 0
      Mij[3, 8] <- 0
      Mij[3, 9] <- 0
      Mij[4, 1] <- 0
      Mij[4, 2] <- 0
      Mij[4, 3] <- 0
      Mij[4, 4] <- m_S1P1C1P1[age] + m_S1P1M1P1[age] + m_S1P1S1P2[age] + m_S1P1C1P2[age] + m_S1P1M1P2[age]
      Mij[4, 5] <- - m_S1P1C1P1[age]
      Mij[4, 6] <- - m_S1P1M1P1[age]
      Mij[4, 7] <- - m_S1P1S1P2[age]
      Mij[4, 8] <- - m_S1P1C1P2[age]
      Mij[4, 9] <- - m_S1P1M1P2[age]
      Mij[5, 1] <- 0
      Mij[5, 2] <- 0
      Mij[5, 3] <- 0
      Mij[5, 4] <- 0
      Mij[5, 5] <- m_C1P1M1P1[age] + m_C1P1C1P2[age] + m_C1P1M1P2[age]
      Mij[5, 6] <- - m_C1P1M1P1[age]
      Mij[5, 7] <- 0
      Mij[5, 8] <- - m_C1P1C1P2[age]
      Mij[5, 9] <- - m_C1P1M1P2[age]
      Mij[6, 1] <- 0
      Mij[6, 2] <- 0
      Mij[6, 3] <- 0
      Mij[6, 4] <- 0
      Mij[6, 5] <- 0
      Mij[6, 6] <- m_M1P1M1P2[age]
      Mij[6, 7] <- 0
      Mij[6, 8] <- 0
      Mij[6, 9] <- - m_M1P1M1P2[age]
      Mij[7, 1] <- 0
      Mij[7, 2] <- 0
      Mij[7, 3] <- 0
      Mij[7, 4] <- 0
      Mij[7, 5] <- 0
      Mij[7, 6] <- 0
      Mij[7, 7] <- m_S1P2C1P2[age] + m_S1P2M1P2[age]
      Mij[7, 8] <- - m_S1P2C1P2[age]
      Mij[7, 9] <- - m_S1P2M1P2[age]
      Mij[8, 1] <- 0
      Mij[8, 2] <- 0
      Mij[8, 3] <- 0
      Mij[8, 4] <- 0
      Mij[8, 5] <- 0
      Mij[8, 6] <- 0
      Mij[8, 7] <- 0
      Mij[8, 8] <- m_C1P2M1P2[age]
      Mij[8, 9] <- - m_C1P2M1P2[age]
      Mij[9, 1] <- 0
      Mij[9, 2] <- 0
      Mij[9, 3] <- 0
      Mij[9, 4] <- 0
      Mij[9, 5] <- 0
      Mij[9, 6] <- 0
      Mij[9, 7] <- 0
      Mij[9, 8] <- 0
      Mij[9, 9] <- 0
      
      Mij <- ifelse(is.na(Mij) | is.infinite(Mij), 0, Mij)
      
      mat_Mij_BC[[age]] <- Mij
    }

  if(union == "S"){
    I <- diag(9) # diag: identity matrix
    radix = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
    lx <- rbind(radix, matrix(rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 9))
    
    for(i in (MinAge - 14):34){
      px <- (I - (0.5 * mat_Mij_BC[[i]])) %*% solve(I + 0.5 * mat_Mij_BC[[i]]) # solve: inverse of ()
      
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
        px <- (I - (0.5 * mat_Mij_BC[[i]][-c(1, 4, 7), -c(1, 4, 7)])) %*% solve(I + 0.5 * mat_Mij_BC[[i]][-c(1, 4, 7), -c(1, 4, 7)]) # solve: inverse of ()
        
        k <- which(i == (MinAge - 14):34)
        lx[k + 1, ] <- lx[k, ] %*% px
      }
      
      colnames(lx) <- c("CP0", "MP0", "CP1", "MP1", "CP2", "MP2")
      
    } else {
      
      I <- diag(3) # diag: identity matrix
      radix = c(1, 0, 0)
      lx <- rbind(radix, matrix(rep(c(0, 0, 0), (49 - MinAge)), nrow = (49 - MinAge), ncol = 3))
      
      for(i in (MinAge - 14):34){
        px <- (I - (0.5 * mat_Mij_BC[[i]][c(3, 6, 9), c(3, 6, 9)])) %*% solve(I + 0.5 * mat_Mij_BC[[i]][c(3, 6, 9), c(3, 6, 9)]) # solve: inverse of ()
        
        k <- which(i == (MinAge - 14):34)
        lx[k + 1, ] <- lx[k, ] %*% px
      }
      
      colnames(lx) <- c("MP0", "MP1", "MP2")
      
    }
  }
  
  return(lx)
  
}

Plotlx <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  Plot1 <- lx1 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge1:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1940s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                                 "#E6550D", "#FDAE6B", "#FEE6CE", 
                                 "#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                                 "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                 "Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot2 <- lx2 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge2:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1950s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                                 "#E6550D", "#FDAE6B", "#FEE6CE", 
                                 "#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                                 "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                 "Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot3 <- lx3 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge3:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
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
    theme(legend.position = "bottom")
  
  
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion of each union and parity status", rot = 90))
  
  return(Allplot)
}

Plotlx_C <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  Plot1 <- lx1 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge1:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1940s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                 "#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                 "Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot2 <- lx2 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge2:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1950s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                 "#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                 "Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot3 <- lx3 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge3:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "Age", y = "", title = "1960s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#E6550D", "#FDAE6B", "#FEE6CE", 
                                 "#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                                 "Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion of each union and parity status", rot = 90))
  
  return(Allplot)
}

Plotlx_M <- function(lx1, lx2, lx3, MinAge1, MinAge2, MinAge3){
  Plot1 <- lx1 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge1:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1940s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot2 <- lx2 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge2:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "", y = "", title = "1950s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  Plot3 <- lx3 %>% 
    as.data.frame() %>% 
    mutate(Age = MinAge3:49) %>%
    gather(key = UP, value = Number, -Age) %>%
    mutate(UP = ifelse(UP < 0, 0, UP),
           UP = factor(UP, levels = c("MP0", "MP1", "MP2"))) %>%
    ggplot(aes(x = Age, y = Number, fill = UP)) +
    geom_area(position = position_stack(reverse = T), stat = "identity") +
    labs(x = "Age", y = "", title = "1960s") +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c("#31A354", "#A1D99B", "#E5F5E0"),
                      labels = c("Married parity 0", "Married parity 1", "Married parity 2"),
                      name = "") +
    theme(legend.position = "bottom")
  
  All <- ggarrange(Plot1, Plot2, Plot3,
                   ncol = 3, nrow = 1,
                   common.legend = T, legend = "bottom")
  
  Allplot <- annotate_figure(All, 
                             left = text_grob("Proportion of each union and parity status", rot = 90))
  
  return(Allplot)
}

#### ---- Read dataset ----
ggs <- read.dta13("Data/HARMONIZED-HISTORIES_ALL_GGSaccess.dta", 
                  nonint.factors = T, convert.factors = T, 
                  select.cols = c("RESPID", "COUNTRY", "BORN_Y", "IBORN_M", "SEX", 
                                  "YEAR_S", "IMONTH_S", "UNION_1", "UNION_Y1", "IUNION_M1", 
                                  "MARR_1", "MARR_Y1", "IMARR_M1", "SEP_1", "SEP_Y1", "ISEP_M1", 
                                  "KID_1", "KID_Y1", "IKID_M1", "KID_2", "KID_Y2", "IKID_M2"))

ggs <- ggs %>% 
  filter(SEX == "Female") %>% 
  mutate_at(c("IMONTH_S", "IBORN_M", "IMARR_M1", "ISEP_M1",
              "IUNION_M1", "IKID_M1", "IKID_M2"),
            funs(case_when(. == "January" ~ 1,
                           . == "February" ~ 2,
                           . == "March" ~ 3,
                           . == "April" ~ 4,
                           . == "May" ~ 5,
                           . == "June" ~ 6,
                           . == "July" ~ 7,
                           . == "August" ~ 8,
                           . == "September" ~ 9,
                           . == "October" ~ 10,
                           . == "November" ~ 11,
                           . == "December" ~ 12))) %>% 
  mutate(int = ((YEAR_S - 1900) * 12) + IMONTH_S,
         birth = ((BORN_Y - 1900) * 12) + IBORN_M,
         age = trunc((int - birth) / 12, 1),
         evermar = ifelse(UNION_1 == "Union of order 1" & MARR_1 == "Marriage", 1, 0),
         everuni = ifelse(UNION_1 == "Union of order 1", 1, 0),
         s_mar1 = ((MARR_Y1 - 1900) * 12) + IMARR_M1,
         s_sep1 = ((SEP_Y1 - 1900) * 12) + ISEP_M1,
         s_uni1 = ((UNION_Y1 - 1900) * 12) + IUNION_M1,
         evercohab = case_when(everuni == 1 & evermar == 0 ~ 1,
                               everuni == 1 & evermar == 1 & (s_uni1 < s_mar1) ~ 1,
                               everuni == 1 & evermar == 1 & (s_uni1 >= s_mar1) ~ 0,
                               everuni == 0 ~ 0),
         s_cohab1 = ifelse(evercohab == 1, s_uni1, NA),
         e_uni1 = case_when(SEP_1 %in% c("Separation", "Death of partner") ~ s_sep1,
                            SEP_1 %in% c("No separation", 
                                         "Became LAT in the same partnership (country specific SWE)") 
                            ~ int),
         e_mar1 = ifelse(evermar == 1, e_uni1, NA),
         e_cohab1 = case_when(evercohab == 1 & evermar == 1 & (s_cohab1 < s_mar1) ~ (s_mar1 - 1),
                              evercohab == 1 & evermar == 0 ~ e_uni1),
         s_child1 = ((KID_Y1 - 1900) * 12) + IKID_M1,
         s_child2 = ((KID_Y2 - 1900) * 12) + IKID_M2,
         #s_child3 = ((KID_Y3 - 1900) * 12) + IKID_M3,
         child1 = ifelse(KID_1 == "Child of order 1", 1, 0),
         child2 = ifelse(KID_2 == "Child of order 2", 1, 0),
         #child3 = ifelse(KID_3 == "Child of order 3", 1, 0),
         num_child = case_when(child2 == 1 ~ 2,
                               child2 == 0 & child1 == 1 ~ 1,
                               child1 == 0 ~ 0),
         sage_cohab1 = case_when(evercohab == 1 & s_cohab1 > 0 ~ trunc((s_cohab1 - birth) / 12, 1),
                                 evercohab == 1 & is.na(s_cohab1) ~ 999,
                                 evercohab == 0 ~ 99),
         eage_cohab1 = case_when(evercohab == 1 ~ trunc((e_cohab1 - birth) / 12, 1),
                                 evercohab == 1 & is.na(e_cohab1) ~ 999,
                                 evercohab == 0 ~ 99),
         sage_mar1 = case_when(evermar == 1 ~ trunc((s_mar1 - birth) / 12, 1),
                               evermar == 1 & is.na(s_mar1) ~ 999,
                               evermar == 0 ~ 99),
         eage_mar1 = case_when(evermar == 1 ~ trunc((e_mar1 - birth) / 12, 1),
                               evermar == 1 & is.na(e_mar1) ~ 999,
                               evermar == 0 ~ 99),
         age_child1 = case_when(KID_1 == "Child of order 1" ~ trunc((s_child1 - birth) / 12, 1),
                                KID_1 == "No child of order 1" ~ 99),
         age_child2 = case_when(KID_2 == "Child of order 2" ~ trunc((s_child2 - birth) / 12, 1),
                                KID_2 == "No child of order 2" ~ 99)#,
         #age_child3 = case_when(KID_3 == "Child of order 3" ~ trunc((s_child3 - birth) / 12, 1),
         #                       KID_3 == "No child of order 3" ~ 99)
  ) %>%
  filter(age >= 35,
         sage_cohab1 %out% c(NA, 999),
         eage_cohab1 %out% c(NA, 999),
         sage_mar1 %out% c(NA, 999),
         eage_mar1 %out% c(NA, 999),
         age_child1 %out% c(NA, 999),
         age_child2 %out% c(NA, 999)) %>% 
  select(RESPID, COUNTRY, BORN_Y, int, birth, age, everuni, evercohab, evermar, s_uni1, s_cohab1, e_cohab1, 
         s_mar1, e_mar1, num_child, child1, child2, s_child1, s_child2, sage_cohab1, 
         eage_cohab1, sage_mar1, eage_mar1, age_child1, age_child2) %>% 
  mutate(order = case_when(num_child == 0 & everuni == 0 ~ "S1P0",
                           num_child == 0 & evercohab == 1 & evermar == 0 ~ "S1P0-C1P0",
                           num_child == 0 & evercohab == 0 & evermar == 1 ~ "S1P0-M1P0",
                           num_child == 0 & evercohab == 1 & evermar == 1 ~ "S1P0-C1P0-M1P0",
                           num_child == 1 & everuni == 0 ~ "S1P0-S1P1",
                           num_child == 1 & evercohab == 1 & evermar == 0 & s_cohab1 == s_child1 ~ "S1P0-C1P1",
                           num_child == 1 & evercohab == 0 & evermar == 1 & s_mar1 == s_child1 ~ "S1P0-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 0 & s_child1 < s_cohab1 ~ "S1P0-S1P1-C1P1",
                           num_child == 1 & evercohab == 0 & evermar == 1 & s_child1 < s_mar1 ~ "S1P0-S1P1-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 1 & s_child1 < s_cohab1 ~ "S1P0-S1P1-C1P1-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 0 & s_cohab1 < s_child1 ~ "S1P0-C1P0-C1P1",
                           num_child == 1 & evercohab == 0 & evermar == 1 & s_mar1 < s_child1 ~ "S1P0-M1P0-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 1 & s_mar1 < s_child1 ~ "S1P0-C1P0-M1P0-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 1 & s_cohab1 < s_child1 & s_child1 < s_mar1 ~ "S1P0-C1P0-C1P1-M1P1",
                           num_child == 1 & evercohab == 1 & evermar == 1 & s_mar1 == s_child1 ~ "S1P0-C1P0-M1P1",
                           num_child == 2 & everuni == 0 ~ "S1P0-S1P1-S1P2",
                           num_child == 2 & evercohab == 1 & evermar == 0 & s_child1 < s_cohab1 & s_cohab1 == s_child2 ~ "S1P0-S1P1-C1P2",
                           num_child == 2 & evercohab == 0 & evermar == 1 & s_child1 < s_mar1 & s_mar1 == s_child2 ~ "S1P0-S1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 0 & s_child2 < s_cohab1 ~ "S1P0-S1P1-S1P2-C1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_child2 < s_cohab1 ~ "S1P0-S1P1-S1P2-C1P2-M1P2",
                           num_child == 2 & evercohab == 0 & evermar == 1 & s_child2 < s_mar1 ~ "S1P0-S1P1-S1P2-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 0 & s_cohab1 == s_child1 ~ "S1P0-C1P1-C1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 == s_child1 & s_mar1 == s_child2 ~ "S1P0-C1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 == s_child1 & s_child2 < s_mar1~ "S1P0-C1P1-C1P2-M1P2",
                           num_child == 2 & evercohab == 0 & evermar == 1 & s_mar1 == s_child1 ~ "S1P0-M1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 0 & s_child1 < s_cohab1 & s_cohab1 < s_child2 ~ "S1P0-S1P1-C1P1-C1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_child1 < s_cohab1 & s_mar1 == s_child2 ~ "S1P0-S1P1-C1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_child1 < s_cohab1 & s_cohab1 < s_child2 ~ "S1P0-S1P1-C1P1-C1P2-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_child1 < s_cohab1 & s_mar1 < s_child2 ~ "S1P0-S1P1-C1P1-M1P1-M1P2",
                           num_child == 2 & evercohab == 0 & evermar == 1 & s_child1 < s_mar1 & s_mar1 < s_child2 ~ "S1P0-S1P1-M1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 0 & s_cohab1 < s_child1 ~ "S1P0-C1P0-C1P1-C1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 < s_child1 & s_mar1 == s_child2 ~ "S1P0-C1P0-C1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 < s_child1 & s_child2 < s_mar1 ~ "S1P0-C1P0-C1P1-C1P2-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 < s_child1 & s_child1 < s_mar1 & s_mar1 < s_child2 ~ "S1P0-C1P0-C1P1-M1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_mar1 == s_child1 ~ "S1P0-C1P0-M1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_mar1 < s_child1 ~ "S1P0-C1P0-M1P0-M1P1-M1P2",
                           num_child == 2 & evercohab == 0 & evermar == 1 & s_mar1 < s_child1 ~ "S1P0-M1P0-M1P1-M1P2",
                           num_child == 2 & evercohab == 1 & evermar == 1 & s_cohab1 == s_child1 & s_mar1 < s_child2 ~ "S1P0-C1P1-M1P1-M1P2"
                           #num_child == 3 & everuni == 0 ~ "SP0-SP1-SP2-SP3",
  ),
  bc = case_when(BORN_Y >= 1930 & BORN_Y <= 1939  ~ "1930s",
                 BORN_Y >= 1940 & BORN_Y <= 1949  ~ "1940s",
                 BORN_Y >= 1950 & BORN_Y <= 1959  ~ "1950s",
                 BORN_Y >= 1960 & BORN_Y <= 1969  ~ "1960s",
                 BORN_Y >= 1970 & BORN_Y <= 1979  ~ "1970s"))

#### ---- Define UI ----
ui <- fluidPage(
  
  # App title
  titlePanel("Union and parity states multistate life tables"),
  
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
  tags$p("This is a supplemental material of the article 'Sociological fertility clock'"),
  tags$p("This interactive tool presents graphs and table showing the results of multistate."),
  
  tags$br(),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      helpText("Choose a country that you want to calculate:"),
      
      # Input: Selector for choosing dataset
      selectInput(inputId = "dataset",
                  label = "Country",
                  choices = c("Austria", "Belarus", "Belgium", "Bulgaria", "Czech Republic", 
                              "Estonia", "France", "Germany", "Hungary", "Italy", 
                              "Lithuania","Netherlands", "Norway", "Poland",
                              "Romania", "Russia", "Spain", "Sweden",
                              "The UK", "The US"),
                  selected = "Sweden"),
      
      #actionButton("update", label = "Decompose"),
      
      helpText("Choose the starting age:"),
      
      sliderInput(inputId = "MinAge", label = "Starting age",
                  min = 15, max = 48, value = 15, step = 1),
      
      # Source
      "Source: ", tags$a(href = "https://www.ggp-i.org/", "Hamonized Histories")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      ## Output: The transition rate of each parity
      plotOutput(outputId = "Plot_lx_S"),
      # Output: Figure title
      textOutput(outputId = "title_plot_S"),
      
      plotOutput(outputId = "Plot_lx_C"),
      # Output: Figure title
      textOutput(outputId = "title_plot_C"),
      
      plotOutput(outputId = "Plot_lx_M"),
      # Output: Figure title
      textOutput(outputId = "title_plot_M"),
      
      tableOutput(outputId = "source")
      
    )
  )
)


#### ---- Server ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  sel_ggsname <- reactive({
    switch(input$dataset,
           "Austria" = "Austria GGS wave1", 
           "Belarus" = "Belarus GGS wave 1", 
           "Belgium" = "Belgium GGS wave1", 
           "Bulgaria" = "Bulgaria GGS wave1", 
           "Czech Republic" = "Czech Republic GGS wave 1", 
           "Estonia" = "Estonia GGS wave1", 
           "France" = "France GGS wave1", 
           "Germany" = c("Germany Pairfam", "Germany GGS wave1"), 
           "Hungary" = "Hungary GGS wave1", 
           "Italy" = "Italy GGS wave1", 
           "Lithuania" = "Lithuania GGS wave1",
           "Netherlands" = c("NDL FFS", "NDL OG 2013"), 
           "Norway" = "Norway GGS wave1", 
           "Poland" = "Poland GGS wave1",
           "Romania" = "Romania GGS wave1", 
           "Russia" = "Russia GGS wave1", 
           "Spain" = "Spain SFS", 
           "Sweden" = "Sweden GGS wave 1",
           "The UK" = "UK BHPS", 
           "The US" = c("USA NSFG 1995", "USA NSFG 2007")
    )
  })
  
  # Read the data-set
  cntr <- reactive({
    
    cntr <- ggs %>% 
      filter(COUNTRY == sel_ggsname())
  })
  
  ## single
  sel_lx_S_1940s <- reactive({
    SWE_15_1940s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1940s", union = "S")
    SWE_15_1940s <- ifelse(SWE_15_1940s < 0, 0, SWE_15_1940s)
  })
  
  sel_lx_S_1950s <- reactive({
    SWE_15_1950s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1950s", union = "S")
    SWE_15_1950s <- ifelse(SWE_15_1950s < 0, 0, SWE_15_1950s)
  })
  
  sel_lx_S_1960s <- reactive({
    SWE_15_1960s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1960s", union = "S")
    SWE_15_1960s <- ifelse(SWE_15_1960s < 0, 0, SWE_15_1960s)
  })
  
  ## cohabitation
  sel_lx_C_1940s <- reactive({
    SWE_15_1940s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1940s", union = "C")
    SWE_15_1940s <- ifelse(SWE_15_1940s < 0, 0, SWE_15_1940s)
  })
  
  sel_lx_C_1950s <- reactive({
    SWE_15_1950s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1950s", union = "C")
    SWE_15_1950s <- ifelse(SWE_15_1950s < 0, 0, SWE_15_1950s)
  })
  
  sel_lx_C_1960s <- reactive({
    SWE_15_1960s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1960s", union = "C")
    SWE_15_1960s <- ifelse(SWE_15_1960s < 0, 0, SWE_15_1960s)
  })
  
  ## married
  sel_lx_M_1940s <- reactive({
    SWE_15_1940s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1940s", union = "M")
    SWE_15_1940s <- ifelse(SWE_15_1940s < 0, 0, SWE_15_1940s)
  })
  
  sel_lx_M_1950s <- reactive({
    SWE_15_1950s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1950s", union = "M")
    SWE_15_1950s <- ifelse(SWE_15_1950s < 0, 0, SWE_15_1950s)
  })
  
  sel_lx_M_1960s <- reactive({
    SWE_15_1960s <- Getlx(cntr = cntr(), MinAge = input$MinAge, BC = "1960s", union = "M")
    SWE_15_1960s <- ifelse(SWE_15_1960s < 0, 0, SWE_15_1960s)
  })
  
  
  
  ### ---- Results and plot ----
  ## Plot lx
  output$Plot_lx_S <- renderPlot({
    Plotlx(lx1 = sel_lx_S_1940s(), MinAge1 = input$MinAge,
           lx2 = sel_lx_S_1950s(), MinAge2 = input$MinAge,
           lx3 = sel_lx_S_1960s(), MinAge3 = input$MinAge)
  })
  # Figure title of lx
  output$title_plot_S <- renderText(paste("Figure 1: The proportion of each family status by age for women in single and parity 0 at age", input$MinAge, "born in 1940s, 1950s, and 1960s", input$dataset))
  
  output$Plot_lx_C <- renderPlot({
    Plotlx_C(lx1 = sel_lx_C_1940s(), MinAge1 = input$MinAge,
             lx2 = sel_lx_C_1950s(), MinAge2 = input$MinAge,
             lx3 = sel_lx_C_1960s(), MinAge3 = input$MinAge)
  })
  # Figure title of lx
  output$title_plot_C <- renderText(paste("Figure 2: The proportion of each family status by age for women in cohabitation and parity 0 at age", input$MinAge, "born in 1940s, 1950s, and 1960s", input$dataset))
  
  output$Plot_lx_M <- renderPlot({
    Plotlx_M(lx1 = sel_lx_M_1940s(), MinAge1 = input$MinAge,
             lx2 = sel_lx_M_1950s(), MinAge2 = input$MinAge,
             lx3 = sel_lx_M_1960s(), MinAge3 = input$MinAge)
  })
  # Figure title of lx
  output$title_plot_M <- renderText(paste("Figure 3: The proportion of each family status by age for women in marriage and parity 0 at age", input$MinAge, "born in 1940s, 1950s, and 1960s", input$dataset))
  
  # Figure source
  output$source <- renderText("Source: Authors’ calculations using the Harmonized Histories.")
}

shinyApp(ui = ui, server = server)