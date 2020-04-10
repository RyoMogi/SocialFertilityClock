library(tidyverse)
library(cowplot)

#### ---- Figure 3 ----
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
  data <- get(name_cntr[i])
  c_lx49 <- Getlx49(Mx = data, union = "S")
  c_lx   <- Getlx(Mx = data, union = "S", MinAge = 15)
  c_lxSP0 <- c_lx[, 1]
  Age <- 15:49
  CountryBC <- rep(name_cntr[i], length(Age))
  c_lx49 <- cbind(Age, CountryBC, c_lx49, c_lxSP0)
  
  lx49 <- rbind(lx49, c_lx49)
}

l30_SP0 <- lx49 %>% 
  as.data.frame() %>% 
  mutate(Age = as.numeric(as.character(Age)),
         c_lx49 = as.numeric(as.character(c_lx49)),
         c_lxSP0 = as.numeric(as.character(c_lxSP0)),
         c_lxSP0 = round(c_lxSP0, 2)) %>%
  filter(Age == 30) %>% 
  select(CountryBC, c_lxSP0)

Dlx49 <- lx49 %>% 
  as.data.frame() %>% 
  mutate(Age = as.numeric(as.character(Age)),
         c_lx49 = as.numeric(as.character(c_lx49)),
         c_lxSP0 = as.numeric(as.character(c_lxSP0)),
         Country = str_extract(CountryBC, "[A-Z]{3}"),
         Country = case_when(Country == "HUN" ~ "Hungary",
                             Country == "ITA" ~ "Italy",
                             Country == "USA" ~ "The US",
                             Country == "BLR" ~ "Belarus",
                             Country == "BGR" ~ "Bulgaria",
                             Country == "ESP" ~ "Spain",
                             Country == "SWE" ~ "Sweden"),
         bc = str_extract(CountryBC, "[0-9]{4}[s]"),
         c_lx49 = round(c_lx49, 2),
         c_lxSP0 = round(c_lxSP0, 2)) %>%
  select(-c_lxSP0) %>% 
  filter(Age %in% c(15, 30)) %>% 
  spread(key = Age, value = c_lx49) %>% 
  rename("Age15" = "15",
         "Age30" = "30") %>% 
  left_join(l30_SP0, by = c("CountryBC"))
#write.csv(Dlx49, "Results/lx49_allcountries.csv")

cp_bw <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
main_lx49 <- Dlx49 %>% 
  mutate(c_lxSP0_cate = case_when(c_lxSP0 < 0.05 ~ "0.05",
                                  c_lxSP0 >= 0.05 & c_lxSP0 < 0.1 ~ "0.1",
                                  c_lxSP0 >= 0.1  & c_lxSP0 < 0.15 ~ "0.15",
                                  c_lxSP0 >= 0.15 ~ "0.2"),
         c_lxSP0_cate = as.factor(c_lxSP0_cate)) %>% 
  ggplot(aes(x = Age15, y = Age30, colour = bc)) +
  geom_point(aes(size = c_lxSP0_cate)) +
  scale_x_continuous(limits = c(0, 0.15)) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(x = expression(paste("Proportion in ", S[0], " by age 50")), y = expression(paste("Proportion ", S[0]," at age 50 among women in ", S[0], " at age 30"))) +
  scale_color_manual(name = "Birth cohort", values = c(cp_bw[1], cp_bw[2], cp_bw[3])) +
  scale_size_manual(labels = c("5", "10", "15", "20"),
                    values = c(1, 3, 6, 10),
                    name = expression(paste("% of ", S[0], " at age 30"))) + 
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  geom_text(data = Dlx49 %>% 
              filter(CountryBC %in% c("ITA1960s", "ESP1960s", "HUN1960s", "BGR1960s", "USA1960s")),
            aes(label = Country), nudge_y = 0.03, show.legend = F) +
  geom_text(data = Dlx49 %>% 
              filter(CountryBC %in% c("SWE1960s")),
            aes(label = Country), nudge_y = 0.02, show.legend = F, size = 2.8) +
  geom_line(data = . %>% 
              filter(CountryBC %in% c("SWE1940s", "SWE1950s", "SWE1960s", "ITA1940s", "ITA1950s", "ITA1960s",
                                      "BGR1940s", "BGR1950s", "BGR1960s")), 
            aes(group = Country), colour = 1)

xdens <- axis_canvas(main_lx49, axis = "x") +
  geom_density(data = Dlx49, aes(x = Age15, fill = bc),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))

ydens <- axis_canvas(main_lx49, axis = "y", coord_flip = T) +
  geom_density(data = Dlx49, aes(x = Age30, fill = bc),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c(cp_bw[1], cp_bw[2], cp_bw[3]))

p1 <- insert_xaxis_grob(main_lx49, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(p2)
ggsave("Graph/lx49_ver3.pdf")


