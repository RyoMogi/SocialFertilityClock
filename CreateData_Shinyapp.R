### This is a code for creating datasets for shinyapp.
### 2019/10/10

library(readstata13)
library(tidyverse)
`%out%` = Negate(`%in%`)
ggs <- read.dta13("Data/HARMONIZED-HISTORIES_ALL_GGSaccess.dta", 
                  nonint.factors = T, convert.factors = T)

#### ---- Get Mx ----
### Austria
AUT1960s <- GetMx(country = "Austria GGS wave1", BC = "1960s")
AUT1970s <- GetMx(country = "Austria GGS wave1", BC = "1970s")

### Belarus
BLR1940s <- GetMx(country = "Belarus GGS wave 1", BC = "1940s")
BLR1950s <- GetMx(country = "Belarus GGS wave 1", BC = "1950s")
BLR1960s <- GetMx(country = "Belarus GGS wave 1", BC = "1960s")

### Belgium
BEL1940s <- GetMx(country = "Belgium GGS wave1", BC = "1940s")
BEL1950s <- GetMx(country = "Belgium GGS wave1", BC = "1950s")
BEL1960s <- GetMx(country = "Belgium GGS wave1", BC = "1960s")

### Bulgaria
BGR1940s <- GetMx(country = "Bulgaria GGS wave1", BC = "1940s")
BGR1950s <- GetMx(country = "Bulgaria GGS wave1", BC = "1950s")
BGR1960s <- GetMx(country = "Bulgaria GGS wave1", BC = "1960s")

### Czech Republic
CZE1940s <- GetMx(country = "Czech Republic GGS wave 1", BC = "1940s")
CZE1950s <- GetMx(country = "Czech Republic GGS wave 1", BC = "1950s")
CZE1960s <- GetMx(country = "Czech Republic GGS wave 1", BC = "1960s")

### Estonia
EST1940s <- GetMx(country = "Estonia GGS wave1", BC = "1940s")
EST1950s <- GetMx(country = "Estonia GGS wave1", BC = "1950s")
EST1960s <- GetMx(country = "Estonia GGS wave1", BC = "1960s")

### France
FRA1940s <- GetMx(country = "France GGS wave1", BC = "1940s")
FRA1950s <- GetMx(country = "France GGS wave1", BC = "1950s")
FRA1960s <- GetMx(country = "France GGS wave1", BC = "1960s")

### Germany
DEU1940s <- GetMx(country = c("Germany Pairfam", "Germany GGS wave1"), BC = "1940s")
DEU1950s <- GetMx(country = c("Germany Pairfam", "Germany GGS wave1"), BC = "1950s")
DEU1960s <- GetMx(country = c("Germany Pairfam", "Germany GGS wave1"), BC = "1960s")

### Hungary
HUN1940s <- GetMx(country = "Hungary GGS wave1", BC = "1940s")
HUN1950s <- GetMx(country = "Hungary GGS wave1", BC = "1950s")
HUN1960s <- GetMx(country = "Hungary GGS wave1", BC = "1960s")

### Italy
ITA1940s <- GetMx_ITA(country = "Italy GGS wave1", BC = "1940s")
ITA1950s <- GetMx_ITA(country = "Italy GGS wave1", BC = "1950s")
ITA1960s <- GetMx_ITA(country = "Italy GGS wave1", BC = "1960s")

### Lithuania
LTU1940s <- GetMx(country = "Lithuania GGS wave1", BC = "1940s")
LTU1950s <- GetMx(country = "Lithuania GGS wave1", BC = "1950s")
LTU1960s <- GetMx(country = "Lithuania GGS wave1", BC = "1960s")

### Netherlands
NLD1940s <- GetMx(country = c("NDL FFS", "NDL OG 2013"), BC = "1940s")
NLD1950s <- GetMx(country = c("NDL FFS", "NDL OG 2013"), BC = "1950s")
NLD1960s <- GetMx(country = c("NDL FFS", "NDL OG 2013"), BC = "1960s")

### Norway
NOR1940s <- GetMx(country = "Norway GGS wave1", BC = "1940s")
NOR1950s <- GetMx(country = "Norway GGS wave1", BC = "1950s")
NOR1960s <- GetMx(country = "Norway GGS wave1", BC = "1960s")

### Poland
POL1940s <- GetMx(country = "Poland GGS wave1", BC = "1940s")
POL1950s <- GetMx(country = "Poland GGS wave1", BC = "1950s")
POL1960s <- GetMx(country = "Poland GGS wave1", BC = "1960s")

### Romania
ROM1940s <- GetMx(country = "Romania GGS wave1", BC = "1940s")
ROM1950s <- GetMx(country = "Romania GGS wave1", BC = "1950s")
ROM1960s <- GetMx(country = "Romania GGS wave1", BC = "1960s")

### Russia
RUS1940s <- GetMx(country = "Russia GGS wave1", BC = "1940s")
RUS1950s <- GetMx(country = "Russia GGS wave1", BC = "1950s")
RUS1960s <- GetMx(country = "Russia GGS wave1", BC = "1960s")

### Spain
ESP1940s <- GetMx(country = "Spain SFS", BC = "1940s")
ESP1950s <- GetMx(country = "Spain SFS", BC = "1950s")
ESP1960s <- GetMx(country = "Spain SFS", BC = "1960s")

### Sweden
SWE1940s <- GetMx(country = "Sweden GGS wave 1", BC = "1940s")
SWE1950s <- GetMx(country = "Sweden GGS wave 1", BC = "1950s")
SWE1960s <- GetMx(country = "Sweden GGS wave 1", BC = "1960s")

### UK
GBR1940s <- GetMx(country = "UK BHPS", BC = "1940s")
GBR1950s <- GetMx(country = "UK BHPS", BC = "1950s")
GBR1960s <- GetMx(country = "UK BHPS", BC = "1960s")

### US
USA1940s <- NULL
USA1950s <- GetMx(country = "USA NSFG 2007", BC = "1950s") # 0 obs from 2007
USA1960s <- GetMx(country = "USA NSFG 2007", BC = "1960s")
USA1970s <- GetMx(country = "USA NSFG 2007", BC = "1970s")

#### ---- Store datasets ----
name_cntr <- c("AUT1960s", "BLR1940s", "BLR1950s", "BLR1960s", "BEL1940s", 
               "BEL1950s", "BEL1960s", "BGR1940s", "BGR1950s", "BGR1960s", "CZE1940s", "CZE1950s", 
               "CZE1960s", "EST1940s", "EST1950s", "EST1960s", "FRA1940s", "FRA1950s", "FRA1960s", 
               "DEU1940s", "DEU1950s", "DEU1960s", "HUN1940s", "HUN1950s", "HUN1960s", "ITA1940s", 
               "ITA1950s", "ITA1960s", "LTU1940s", "LTU1950s", "LTU1960s", "NLD1940s", "NLD1950s", 
               "NLD1960s", "NOR1940s", "NOR1950s", "NOR1960s", "POL1940s", "POL1950s", "POL1960s", 
               "ROM1940s", "ROM1950s", "ROM1960s", "RUS1940s", "RUS1950s", "RUS1960s", "ESP1940s", 
               "ESP1950s", "ESP1960s", "SWE1940s", "SWE1950s", "SWE1960s", "GBR1940s", "GBR1950s", 
               "GBR1960s", "USA1960s")

for(i in 1:length(name_cntr)){
  data <- get(name_cntr[i])
  saveRDS(data, file = paste("Shinyapp_multi/Data/", name_cntr[i], ".Rda", sep = ""))
}
