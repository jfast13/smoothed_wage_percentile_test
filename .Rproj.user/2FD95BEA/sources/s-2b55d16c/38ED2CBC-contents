library(realtalk)
library(epiextractr)
library(tidyverse)
library(here)
library(epidatatools)
library(MetricsWeighted)
library(fixest)
library(bea.R)
library(blsR)
library(data.table)
library(formattable)
library(dplyr)
library(patchwork)
library(scales)

current_year <- 2023
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage",
              "emp", "tc_weekpay", "lfstat",
              "ptecon", "discwork", "unemp", "unempdur", "lookdur", "nilf", "a_earnhour", "a_weekpay", "wbho")


#### DATA IMPORTATION ####
# Load data for the "may" period (pre-1978)
may <- load_may(1973:1978, all_of(c(var_list, "finalwgt"))) %>% mutate(sample = "may")

# Load Outgoing Rotation Group (ORG) data
org <- load_org(1979:current_year, all_of(c(var_list, "orgwgt", "finalwgt"))) %>% mutate(sample = "org")


# standard restrictions
data <- bind_rows(org, may)  %>% 
  # Age and selfemp restrictions
  filter(selfemp == 0, age >= 16, !is.na(wage),
         case_when(selfinc == 0 & !is.na(selfinc) ~ TRUE, # filter selfinc == 0 for year >= 1989
                   # keep any year that doesn't have selfinc (selfinc is NA)
                   is.na(selfinc) ~ TRUE, 
                   # exclude all other cases
                   TRUE ~ FALSE))

## CPI ## 
cpi <- read.csv(here("data/cpi_annual.csv"))

cpi_base <- cpi$cpiurs[cpi$year == current_year]
cpi_wage <- cpi$cpiurs[cpi$year == 2023]

#### MASTER WAGE DATASET ####
# master wage dataset
wage_master <- data %>%
  # merge CPI-U-RS
  left_join(cpi, by = "year") %>%
  # inflation adjust wages to current years dollars
  mutate(realwage = wage * (cpi_base/cpiurs),
         adj_wage = wage * (cpi_wage/cpiurs)) %>%
  # flatten labels and adjust weight variables
  mutate(# annual weight adjustment
         wgt = case_when(
           # may data wgt ~ finalwgt
           year < 1979 ~ finalwgt,
           year >= 1979 ~ orgwgt/12))%>%
  arrange(year)

wage_master_no_imputed <- data %>%
  # merge CPI-U-RS
  left_join(cpi, by = "year") %>%
  filter(!(a_earnhour == 1 & !is.na(a_earnhour)), 
         !(a_weekpay == 1 & !is.na(a_weekpay))) %>% 
  # inflation adjust wages to current years dollars
  mutate(realwage = wage * (cpi_base/cpiurs),
         adj_wage = wage * (cpi_wage/cpiurs)) %>%
  # flatten labels and adjust weight variables
  mutate(# annual weight adjustment
    wgt = case_when(
      # may data wgt ~ finalwgt
      year < 1979 ~ finalwgt,
      year >= 1979 ~ orgwgt/12))%>%
  arrange(year)



# Runinng the code 
source(here("code/medianwage_test.R"), echo = TRUE)
source(here("code/Visualizations.R"), echo = TRUE)

