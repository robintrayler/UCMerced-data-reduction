rm(list = ls())
## load required packages------------------------------------------------------
library(tidyverse)

## load data-------------------------------------------------------------------
dat <- read_csv('~/Dropbox/Documents/Geology/Post Doc/Research/GasBench/results/GB20190520.csv')
threshold <- 120

## linearity correction--------------------------------------------------------
## add selector to select standard and number
lin <- dat %>%
  filter(Rt > threshold & `Identifier 2` == 'NBS 18') %>% 
  select(`Identifier 1`,
         `Identifier 2`, 
         `d 13C/12C` , 
         `d 18O/16O`,
         `Ampl. 44`) %>% 
  group_by(`Identifier 1`)

## fit a line------------------------------------------------------------------
fit.carbon <- lm(`d 13C/12C` ~ `Ampl. 44`, data = lin)
fit.oxygen <- lm(`d 18O/16O` ~ `Ampl. 44`, data = lin)
plot(lin$`Ampl. 44`, lin$`d 18O/16O`)
## correct the carbon data-----------------------------------------------------
corr <- dat %>% filter(Rt > threshold) %>% 
  select(`Identifier 1`,
         `Identifier 2`,
         `Ampl. 44`,
         `d 13C/12C` , 
         `d 18O/16O`) %>%
  ## add columns with the linearity corrected data
  mutate(d13C.lin.corr = 
           `d 13C/12C` -  (fit.carbon$coefficients[2] * `Ampl. 44` + 
                             fit.carbon$coefficients[1]) + -5.04,
         d18O.lin.cor = `d 18O/16O` - (fit.oxygen$coefficients[2] * `Ampl. 44` + 
                                         fit.oxygen$coefficients[1]) + -23.2) %>%
  ## group and summarize-------------------------------------------------------
group_by(`Identifier 1`) %>%
  summarise(d13C = mean(d13C.lin.corr),
            SD.d13C = sd(d13C.lin.corr),
            d18O = mean(`d 18O/16O`),
            SD.d18O = sd(`d 18O/16O`),
            `Identifier 2` = unique(`Identifier 2`))
