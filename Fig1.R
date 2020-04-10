library(tidyverse)
library(ggpubr)

#### ---- Figure 1 ----

### Sweden
SWE1940s <- GetMx(country = "Sweden GGS wave 1", BC = "1940s")
SWE1950s <- GetMx(country = "Sweden GGS wave 1", BC = "1950s")
SWE1960s <- GetMx(country = "Sweden GGS wave 1", BC = "1960s")

SWE_15_1960s <- Getlx(Mx = SWE1960s, MinAge = 15, union = "S")
SWE_30_1960s <- Getlx(Mx = SWE1960s, MinAge = 30, union = "S")

SWE_lx49_1940s <- Getlx49(Mx = SWE1940s, union = "S")
SWE_lx49_1950s <- Getlx49(Mx = SWE1950s, union = "S")
SWE_lx49_1960s <- Getlx49(Mx = SWE1960s, union = "S")

#write.csv(SWE_15_1960s, "Results/lx_SWE_15_1960s.csv")
#write.csv(SWE_30_1960s, "Results/lx_SWE_30_1960s.csv")

### US
USA1960s <- GetMx(country = "USA NSFG 2007", BC = "1960s")

USA_15_1960s <- Getlx(Mx = USA1960s, MinAge = 15, union = "S")
USA_30_1960s <- Getlx(Mx = USA1960s, MinAge = 30, union = "S")

USA_lx49_1960s <- Getlx49(Mx = USA1960s, union = "S")

#write.csv(USA_15_1960s, "Results/lx_USA_15_1960s.csv")
#write.csv(USA_30_1960s, "Results/lx_USA_30_1960s.csv")

### Plot
Age <- 15:49
lx49 <- cbind(Age, SWE_lx49_1940s, SWE_lx49_1950s, SWE_lx49_1960s, USA_lx49_1960s)
#write.csv(lx49, "Results/lx49_SWE-US.csv")

#PL_lx49 <- lx49 %>% 
#  as.data.frame() %>% 
#  gather(key = bc, value = lx49, -1) %>% 
#  mutate(Country = ifelse(bc == "USA_lx49_1960s", "The US", "Sweden"),
#         bc = case_when(bc == "SWE_lx49_1940s" ~ "1940s",
#                        bc == "SWE_lx49_1950s" ~ "1950s",
#                        bc %in% c("SWE_lx49_1960s", "USA_lx49_1960s") ~ "1960s")) %>% 
#  ggplot(aes(x = Age, y = lx49)) +
#  geom_line(aes(linetype = Country, colour = bc), size = 1.2) +
#  #scale_linetype_manual(values = c("solid", "dash", "dotdash")) +
#  ylab("Proportion of remaining single and in parity 0 at age 49") +
#  scale_colour_discrete(name = "Birth cohort")

PL_SWE_15_1960s <- SWE_15_1960s %>% 
  as.data.frame() %>% 
  mutate(Age = 15:49) %>%
  gather(key = UP, value = Number, -Age) %>%
  mutate(UP = ifelse(UP < 0, 0, UP),
         UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
  ggplot(aes(x = Age, y = Number, fill = UP)) +
  geom_area(position = position_stack(reverse = T), stat = "identity") +
  labs(title = "Sweden", y = "") +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                               "#E6550D", "#FDAE6B", "#FEE6CE", 
                               "#31A354", "#A1D99B", "#E5F5E0"),
                    labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                               "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                               "Married parity 0", "Married parity 1", "Married parity 2"),
                    name = "") +
  theme(legend.position = "bottom", axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

SWE_15_1960s[35, 2] + SWE_15_1960s[35, 5] + SWE_15_1960s[35, 8]

PL_SWE_30_1960s <- SWE_30_1960s %>% 
  as.data.frame() %>% 
  mutate(Age = 30:49) %>%
  gather(key = UP, value = Number, -Age) %>%
  mutate(UP = ifelse(UP < 0, 0, UP),
         UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
  ggplot(aes(x = Age, y = Number, fill = UP)) +
  geom_area(position = position_stack(reverse = T), stat = "identity", na.rm = T) +
  labs(x = "Age", y = "") +
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

PL_USA_15_1960s <- USA_15_1960s %>% 
  as.data.frame() %>% 
  mutate(Age = 15:49) %>%
  gather(key = UP, value = Number, -Age) %>%
  mutate(UP = ifelse(UP < 0, 0, UP),
         UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
  ggplot(aes(x = Age, y = Number, fill = UP)) +
  geom_area(position = position_stack(reverse = T), stat = "identity") +
  labs(title = "The US", y = "") +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = c("#3182BD", "#9ECAE1", "#DEEBF7", 
                               "#E6550D", "#FDAE6B", "#FEE6CE", 
                               "#31A354", "#A1D99B", "#E5F5E0"),
                    labels = c("Single parity 0", "Single parity 1", "Single parity 2",
                               "Cohabitation parity 0", "Cohabitation parity 1", "Cohabitation parity 2",
                               "Married parity 0", "Married parity 1", "Married parity 2"),
                    name = "") +
  theme(legend.position = "bottom", axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

USA_15_1960s[35, 2] + USA_15_1960s[35, 5] + USA_15_1960s[35, 8]

PL_USA_30_1960s <- USA_30_1960s %>% 
  as.data.frame() %>% 
  mutate(Age = 30:49) %>%
  gather(key = UP, value = Number, -Age) %>%
  mutate(UP = ifelse(UP < 0, 0, UP),
         UP = factor(UP, levels = c("SP0", "SP1", "SP2", "CP0", "CP1", "CP2", "MP0", "MP1", "MP2"))) %>%
  ggplot(aes(x = Age, y = Number, fill = UP)) +
  geom_area(position = position_stack(reverse = T), stat = "identity") +
  labs(x = "Age", y = "") +
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

lx1530_1960s <- ggarrange(PL_SWE_15_1960s, PL_USA_15_1960s, 
                          PL_SWE_30_1960s, PL_USA_30_1960s,
                          nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
annotate_figure(lx1530_1960s, 
                left = text_grob("Proportion in each union and parity status", rot = 90))
ggsave("Graph/lx_1960s_SWE-US.pdf")


#ggarrange(lx1530_1960s, PL_lx49, ncol = 1, nrow = 2, 
#          labels = c("A", "B"), label.x = 0.03, label.y = c(1, 1.08))
#ggsave("Graph/lx-lx49_1960s.pdf")


##### ---- dx ----
#### Sweden
#SWE1960s_dx <- Getdx(country = "Sweden GGS wave 1", BC = "1960s")
#
#### US
#USA1960s_dx <- Getdx(country = "USA NSFG 2007", BC = "1960s")

## % of ever-cohab
#sum(colSums(SWE1960s_dx[7:10, ])) / colSums(SWE1960s_dx)[1]
#sum(colSums(USA1960s_dx[7:10, ])) / colSums(USA1960s_dx)[1]
#
## % of ever-mar
#sum(colSums(SWE1960s_dx[c(12:15, 18:20), ])) / colSums(SWE1960s_dx)[1]
#sum(colSums(USA1960s_dx[c(12:15, 18:20), ])) / colSums(USA1960s_dx)[1]
