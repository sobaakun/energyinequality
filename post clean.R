library(haven)
library(tidyverse)
library(ineq)
tempfile <- read_dta("tempfile.dta")
tempfile[is.na(tempfile)] <- 0
mainfile <- templfile %>%
  select(psu_number, hh_number, prov, hhsize, ad_4, coalexpyear, keroseneexpyear,
         lpgasexpyear, otherfuelexpyear, totrooms, foundation, roof, ownorrented, 
         electricityexpenseyear, firewoodexpensebuy, firewoodexpenseonwland, 
         firewoodexpensecommforest, firewoodexpensegovforest, firewoodexpenseother,
         educationhhh, cashtransferreceived, totagrincome, incomelivestock, 
         netrevenue, moneyreceivedfromabsentee, totalremittancereceived, 
         incomeothersource, moneysenttoabsentee
  )
#Total Energy Expenditure
mainfile <- mainfile %>%
  mutate(totalenergyexp = coalexpyear + electricityexpenseyear + keroseneexpyear +
           firewoodexpensebuy + firewoodexpenseownland + firewoodexpensecommforest +
           firewoodexpensegovforest + firewoodexpenseother + lpgasexpyear)
#Energy expense ratio to Total Income 
tempfile <- tempfile %>% 
  mutate(
    energyprop = totalenergyexp / totalincome
  )
tempfile[is.na(tempfile)] <- 0 
tempfile[sapply(tempfile, is.infinite)] <- 0
#Rural-Urban Dummy 
tempfile <- tempfile %>% 
  mutate(
    area = ifelse(ad_4 == 3, 1, 0)
  )
#Total Firewood Consumption 
tempfile <- tempfile %>%
  
#Lorenz Curve
electricity_lorenz = Lc(tempfile$electricityexpenseyear)
plot(electricity_lorenz, 
     main = "Lorenz Curve", 
     col = "blue", 
     lwd = 2, 
     xlab = "Cumulative Share of Population", 
     ylab = "Cumulative Share of Electricity Consumption"
)

lpg_lorenz = Lc(tempfile$lpgasexpyear)
plot(lpg_lorenz, 
     main = "Lorenz Curve", 
     col = "red", 
     lwd = 2, 
     xlab =  "Cumulative Share of Population", 
     ylab = "Cumulative Share of LPG Consumption"
)
#Regression to establish relationships 
reg_model <- lm(
  lpgasexpyear ~ hhsize + totrooms + area + educationhhh + totalincome, data = tempfile
)



  