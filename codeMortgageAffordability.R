
# inspiration from here https://reddit.com/r/dataisbeautiful/s/Y3jZEf5CvZ

rm(list = ls()) #start with empty workspace 

library(tidyverse)
library(data.table)
library(fredr)
#library(quantmod)
library(scales)

dsrFolder <- "~/GitHub/mortgageAffordability"
ifelse(
  !dir.exists(file.path(dsrFolder))
  , dir.create(file.path(dsrFolder))
  , FALSE)

setwd(dsrFolder)

theme_set(theme_minimal())

if(FALSE) fredr_series_search_text(
  search_text = "Real Median") %>%
  view("SeriesInfo")

MEHOINUSCAA672N <- fredr(
  series_id = "MEHOINUSCAA672N",
  observation_start = as.Date("1990-01-01")) %>% 
  mutate(st = "CA") %>% 
  data.table::setattr(., "comment","Annual, Real Median Household Income in California")

# MEHOINUSARA672N <- fredr(
#   series_id = "MEHOINUSARA672N",
#   observation_start = as.Date("1990-01-01")) %>% 
#   mutate(st = "AR") %>% 
#   data.table::setattr(., "comment","Annual, Real Median Household Income in Arkansas")
# 
# MEHOINUSTXA672N <- fredr(
#   series_id = "MEHOINUSTXA672N",
#   observation_start = as.Date("1990-01-01")) %>% 
#   mutate(st = "TX") %>% 
#   data.table::setattr(., "comment","Annual, Real Median Household Income in Texas")

MEDLISPRICA <- fredr(
  series_id = "MEDLISPRICA",
  observation_start = as.Date("1990-01-01")) %>% 
  data.table::setattr(., "comment","Monthlyl Housing Inventory: Median Listing Price in California")

MORTGAGE30US <- fredr(
  series_id = "MORTGAGE30US",
  observation_start = as.Date("1990-01-01")) %>% 
  data.table::setattr(., "comment","Weekly 30-Year Fixed Rate Mortgage Average in the United States")

# White_Fang_
# u/White_Fang_ avatar
# White_Fang_
# u/White_Fang_
# Sep 20, 2015
# 635
# I created a mortgage affordability index for the US and graphed the results over the past 20 years. The index looks at the ratio of median monthly household income to an average 30-year fixed rate monthly mortgage payment.
# 
# Underlying data:
# Median Household Income (US): https://fred.stlouisfed.org/series/MEHOINUSA646N
# Median Sales Price of Houses Sold (US): https://fred.stlouisfed.org/series/MSPUS
# 30-Year Fixed Rate Mortgage Average in the US: https://fred.stlouisfed.org/series/MORTGAGE30US
# 
# Notes:
# Monthly mortgage payment based on the average 30-year fixed rate for that year with 20% down.
# Median Household Income (US) data is not yet available for 2022 and 2023, so these are projected at 3.0% annual from 2021.
# 2023 data not yet complete.
# Data manipulated in Excel.

attributes(MEHOINUSCAA672N)$comment
attributes(MEDLISPRICA)$comment
attributes(MORTGAGE30US)$commen

# take the monthly median price and get the principal mortgage rate and estimate the
# loan payment Interest plus principal

MEDLISPRICA %>% 
  select( date, Mnth_Hsng_Inv_Medn_Lstn_Pric_in_Cal = value) %>% 
  arrange(desc(date)) %>% 
  left_join(MORTGAGE30US %>% select( date, Mnth_30_Yr_Fixd_Rate_Mrtg_Avrg = value)) %>%
  mutate(Mnth_30_Yr_Fixd_Rate_Mrtg_Avrg = zoo::na.locf(Mnth_30_Yr_Fixd_Rate_Mrtg_Avrg, na.rm = FALSE),
         monthly_rate = Mnth_30_Yr_Fixd_Rate_Mrtg_Avrg / 100 / 12,      # from here
         r = (1 + monthly_rate) ^ 360 - 1,                            #https://masterr.org/r/calculate-mortgage-payment-schedule/
         payment = Mnth_Hsng_Inv_Medn_Lstn_Pric_in_Cal * monthly_rate * (r + 1) / r) -> estPrincipPayment

# get the ratio of payment over the HH income

estPrincipPayment %>% 
  left_join(MEHOINUSCAA672N %>% 
              select(date,  Real_Medn_Hshl_Incm_in_Cal = value)) %>% 
  fill(Real_Medn_Hshl_Incm_in_Cal, .direction = "downup") %>% 
  mutate(mo_payment_ovr_Mo_HHInc = payment/(Real_Medn_Hshl_Incm_in_Cal/12)) -> dat


dat %>% 
  ggplot() +
  geom_line(aes(x = date, y = mo_payment_ovr_Mo_HHInc))


library(strucchange)

dat %>% arrange(dat) %>% 
  mutate(id = row_number()) -> dat

tsMo <- ts(dat$mo_payment_ovr_Mo_HHInc, frequency = 12
           , start = c(min(dat$date) %>% as.Date() %>%  format(., "%Y") %>% as.numeric(),
                       min(dat$date) %>% as.Date() %>%  format(., "%m") %>% as.numeric() ))

# complete structural change analysis on this variable

bpData <- breakpoints(tsMo ~ 1)
# summary(bpData)

breakPoints <- tryCatch( confint(bpData), 
                         error = function(err)  data.frame( `2.5 %` = NA,
                                                            breakpoints = NA,
                                                            `97.5 %` = NA)) %>% 
  .$confint %>% 
  {if(!is.null(.)){
    as.data.frame(.) %>% 
      mutate(rows = row_number()) %>% 
      pivot_longer(-c(rows), names_to = "breakpoint", values_to = "id") %>% 
      left_join(dat %>% select( id, date ))
    
  }}

breakPoints %>%
  select(-c( id )) %>% 
  group_by(rows) %>% 
  pivot_wider(names_from = breakpoint, values_from = date) %>% 
  setNames(gsub("[^[:alnum:]]", perl = TRUE, "", names(.))) %>% 
  left_join(  breakPoints %>% filter(breakpoint == "breakpoints") %>% 
                select(id, breakpoints = date) ) -> wideBreakPoints

dat %>% 
  left_join(wideBreakPoints) %>% 
  mutate(rows = zoo::na.locf(rows, na.rm = FALSE),
         rows = replace_na(rows,0) ) -> dat
dat %>% 
  fwrite("./data.txt")

dat %>% 
  ggplot() +
  geom_line(aes(x = date, y = mo_payment_ovr_Mo_HHInc), alpha = .7) +
  geom_smooth(se = FALSE, method = lm, linetype = 'twodash', color = alpha("#31a354", 0.8),
              aes(x = date, y = mo_payment_ovr_Mo_HHInc, group = rows)) +
  geom_vline(aes(xintercept = as.Date("2020-03-04")),
             color = "blue", linewidth = .1
             ,linetype = "dashed"
             ,alpha = .8) +
  annotate(geom = "text", x = as.Date("2020-03-04"), y = .5,
           colour = "grey25",
           label = "March 4, 2021\nGovernor Newsom\ndeclares a proclamation of\n state of emergency\nover COVID19",
           hjust = 1) +
  geom_vline(aes(xintercept = as.Date("2023-02-28")),
             color = "blue", linewidth = .1
             ,linetype = "dashed"
             ,alpha = .8) +
  annotate(geom = "text", x = as.Date("2023-02-28"), y = .5,
           colour = "grey25",
           label = "February 28, 2023\nGovernor Newsom\ndeclares termination of\n state of emergency\nover COVID19",
           hjust = 1) +
  labs(title = "Mortgage Affordability Index for California Homes and Household Income",
       x = "Month",
       y ="Ratio Monthly Payment Over Monthly Household Income")

# https://public.tableau.com/app/profile/m.ev1333/viz/MorgageAffordabilityIndexForCaliforniaHomes/Dashboard1#1