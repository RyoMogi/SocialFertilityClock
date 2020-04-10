library(tidyverse)

#### ---- Figure 2  ----
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

name_cntr1960s <- c("BLR1960s", "BEL1960s", "BGR1960s", "CZE1960s", "EST1960s", "FRA1960s", 
                    "DEU1960s", "HUN1960s", "ITA1960s", "LTU1960s", "NLD1960s", "NOR1960s", 
                    "POL1960s", "ROM1960s", "RUS1960s", "ESP1960s", "SWE1960s", "GBR1960s", "USA1960s")

Avglx49 <- lx49 %>% 
  as.data.frame() %>% 
  mutate(Age = as.numeric(as.character(Age)),
         c_lx49 = as.numeric(as.character(c_lx49)),
         c_lxSP0 = as.numeric(as.character(c_lxSP0)),
         c_lxSP0 = round(c_lxSP0, 2)) %>%
  filter(CountryBC %in% name_cntr1960s) %>% 
  group_by(Age) %>% 
  summarise(Avg = mean(c_lx49))

#lx49 %>% 
#  as.data.frame() %>% 
#  mutate(Age = as.numeric(as.character(Age)),
#         c_lx49 = as.numeric(as.character(c_lx49)),
#         c_lxSP0 = as.numeric(as.character(c_lxSP0)),
#         c_lxSP0 = round(c_lxSP0, 2)) %>%
#  filter(CountryBC %in% name_cntr1960s,
#         Age == 38) %>% 
#  arrange(desc(c_lx49))

## Biological fertility clock from Leridon (2008)
#Age <- 25:59
ster <- c(10, 11, 12, 14, 17, 20, 24, 29, 35, 42, 
          51, 64, 82, 105, 133, 166, 204, 249, 306, 401, 
          546, 685, 785, 855, 895, 919, 937, 952, 965, 976,
          985, 991, 996, 999, 1000)
prop_ster <- ster * 0.001
prop_ster <- c(rep(0, 10), prop_ster[1:25])

plot_lx49 <- lx49 %>% 
  as.data.frame() %>% 
  mutate(Age = as.numeric(as.character(Age)),
         c_lx49 = as.numeric(as.character(c_lx49)),
         c_lxSP0 = as.numeric(as.character(c_lxSP0)),
         c_lxSP0 = round(c_lxSP0, 2)) %>%
  filter(CountryBC %in% name_cntr1960s) %>% 
  mutate(Country = case_when(CountryBC == "HUN1960s" ~ 19,
                             CountryBC == "NOR1960s" ~ 18,
                             CountryBC == "FRA1960s" ~ 17,
                             CountryBC == "BGR1960s" ~ 16,
                             CountryBC == "BLR1960s" ~ 15,
                             CountryBC == "ROM1960s" ~ 14,
                             CountryBC == "CZE1960s" ~ 13,
                             CountryBC == "RUS1960s" ~ 12,
                             CountryBC == "SWE1960s" ~ 11,
                             CountryBC == "EST1960s" ~ 10,
                             CountryBC == "POL1960s" ~ 9,
                             CountryBC == "USA1960s" ~ 8,
                             CountryBC == "LTU1960s" ~ 7,
                             CountryBC == "NLD1960s" ~ 6,
                             CountryBC == "GBR1960s" ~ 5,
                             CountryBC == "DEU1960s" ~ 4,
                             CountryBC == "ITA1960s" ~ 3,
                             CountryBC == "BEL1960s" ~ 2,
                             CountryBC == "ESP1960s" ~ 1)) %>% 
  select(Age, CountryBC, c_lx49) %>% 
  spread(key = CountryBC, value = c_lx49, -1)

pdf("Graph/lx49_allcountries_ver2.pdf")
plot(plot_lx49$Age, plot_lx49$HUN1960s, type = "l", ylim = c(0, 1), col = 1,
     xlab = expression(paste("Age at which women were single and childless (", S[0], ")")), 
     ylab = expression(paste("Proportion remaining single and childless (", S[0], ") by age 50")))
lines(plot_lx49$Age, plot_lx49$NOR1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$FRA1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$BGR1960s, col = 1)
lines(plot_lx49$Age, plot_lx49$BLR1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$ROM1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$CZE1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$RUS1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$SWE1960s, col = 1)
lines(plot_lx49$Age, plot_lx49$EST1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$POL1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$USA1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$LTU1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$NLD1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$GBR1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$DEU1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$ITA1960s, col = "grey")
lines(plot_lx49$Age, plot_lx49$BEL1960s, col = 1)
lines(plot_lx49$Age, plot_lx49$ESP1960s, col = 1)
lines(Avglx49$Age, Avglx49$Avg, lwd = 4, col = "#ff0303")
text(x = 28.5, y = 0.18, "Spain")
text(x = 30, y = 0.45, "Belgium")
text(x = 42, y = 0.84, "Sweden")
text(x = 19.5, y = 0.25, "Bulgaria")
text(x = 27.5, y = 0.76, "Hungary")
#text(x = 26, y = 0.94, "Social fertility clock", col = "#ff0303", cex = 1.1)
lines(Avglx49$Age, prop_ster, lwd = 3, col = "#238B45", lty = 2)
#text(x = 43, y = 0.10, "Biological fertility clock", col = "#238B45", cex = 1.1)
dev.off()
