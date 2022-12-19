########################################################
#########3 Packages that we should use #################
########################################################

library(RSQLite)
library(tidyverse)
library(tidymodels) 
library(dplyr)
library(furrr) 
library(lubridate)
library(kableExtra)
library(tidyquant)
library(Quandl)
library(readxl)
########################################################
################ NOT IN USE ATM. #######################
########################################################

library(glmnet)
library(broom)
library(timetk)
library(scales)

library(scales)
library(frenchdata)
library(caret)
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(mltools)
library(data.table)
library(vtable)
library(keras)

#######################################################
###### Loading and cleaning data from FRED ############
#######################################################

start_date <- ymd("1986-01-01")
end_date <- ymd("2021-12-31")

fred_data <- tq_get(c("TB3MS","T10Y2YM","T10YFFM","TB6SMFFM","AAA","FEDFUNDS","EXJPUS","EXCHUS"
                      ,"EXUSUK","VIXCLS","GVZCLS","SOANDI","KCFSI","CFNAI","CANDH","MICH","REAINTRATREARAT10Y","EXPINF2YR","EXPINF10YR","MCOILWTICO"),
                    get = "economic.data",
                    from = start_date,
                    to = end_date) %>%
  mutate(
    month = floor_date(date, "month")
  ) %>%
  group_by(symbol,month) %>%
  summarise(ratio = mean(price, na.rm = TRUE))


fred_data_wider <- fred_data %>% 
  group_by(symbol,month) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = symbol,
                     values_from = ratio
  ) %>% select(-row) %>%
  arrange(month)

#######################################################
###### Loading and cleaning data from NASDAQ ##########
#######################################################

shiller_pe <- Quandl(c("MULTPL/SHILLER_PE_RATIO_MONTH"), collapse = "monthly" ,api_key="3v89pZLq1w4K7iw5psi8") %>%
  mutate(month = floor_date(Date, "month")) %>% select(-Date)

div <- Quandl("MULTPL/SP500_DIV_MONTH", api_key="3v89pZLq1w4K7iw5psi8") %>%
  mutate(month = floor_date(Date, "month")) %>% select(-Date)

#######################################################
###### Loading and cleaning data from QPR #############
#######################################################

mom <- read_excel("data/AQR_mom.xlsx") %>%
  rename(mom12_largecap = "U.S. Large Cap", mom12_smallcap = "U.S. Small Cap") %>%
  mutate(
    month = as.Date(floor_date(Month, "month"))) %>% 
  select(-International,-Month)

#############################################################
######### Loading and cleaning data from CRSP ###############
#############################################################

tidy_finance_ML <- dbConnect(SQLite(), "data/tidy_finance_ML.sqlite", 
                             extended_types = TRUE)
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite", 
                          extended_types = TRUE)

compustat <- tbl(tidy_finance, "compustat") %>%
  collect()

be <- compustat %>%
  select(gvkey, datadate, be) %>%
  drop_na()

#Loading in factors_ff_monthly dataset
factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>%
  collect() %>%
  mutate(month = as.Date(as.POSIXct.Date(month)))


factors_q_monthly <- tbl(tidy_finance, "factors_q_monthly") %>%
  collect() %>%
  mutate(month = as.Date(as.POSIXct.Date(month)))

#Loading characteristics and macro variables from CRSP
characteristics_1 <- tbl(tidy_finance, "crsp_monthly")%>%
  collect() %>%
  mutate(month = as.Date(as.POSIXct.Date(month))) 

characteristics = characteristics %>% left_join(characteristics_1 %>% select(month,permno,mktcap), by = c("month","permno"))

  select(month, 
         permno, 
         ret_excess, 
         mktcap_lag, 
        # characteristic_beta, 
        # characteristic_turn, 
        # characteristic_mom1m,   
         characteristic_mom12m, 
        # characteristic_mom36m, 
         macro_dp,
         macro_dy,
         macro_infl,
         macro_de,
         macro_svar,
         macro_ep, 
         macro_bm, 
         macro_ntis,
         macro_tbl,
         macro_lty,
         macro_ltr,
         macro_tms,
         macro_dfy) %>%
  collect() %>%
  mutate(month = as.Date(as.POSIXct.Date(month))) 


characteristics <- characteristics %>%
  left_join(factors_ff_monthly, by = 'month')

mom_data <- characteristics %>%
  select(month, permno, characteristic_mom12m, mktcap_lag, ret_excess, mkt_excess)

macro_vars <- characteristics %>% filter(permno == "10145") %>% select(-permno,-characteristic_mom12m)


features <- macro_vars %>%
  left_join(factors_q_monthly, by ="month") %>%
  left_join(fred_data_wider, by = "month") %>%
  left_join(shiller_pe, by = "month") %>%
  left_join(div, by = "month") %>%
  left_join(mom, by = "month")

write_excel_csv(features,"data/features.csv")
write_excel_csv(mom_data,"data/characteristics.csv")


dbDisconnect(tidy_finance_ML,tidy_finance)
remove(tidy_finance_ML,tidy_finance,factors_ff_monthly,factors_q_monthly,tidy_finance,tidy_finance_ML,fred_data,fred_data_wider,div,macro_vars,mom,shiller_pe,TERMCBPER24NS)
dbListTables(tidy_finance_ML)
