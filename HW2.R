#John Wojciechowski
#ENVST 325 HW 2
#2/9/2026

install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

##Activity 1
streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

str(streamH)

siteInfo

# join site info and stream heights into a new data frame floods
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier
head(floods)

floods_left <- left_join(streamH, siteInfo)
head(floods_left)

floods_inner <- inner_join(streamH, siteInfo)
head(floods_inner)

##In this case it does not appear that the different
## types of join make a difference in the outcome.

##Prompt 2

floods$dateF <- ymd_hm(floods$datetime, tz = "America/New_York")

##Prompt 3

earliest_flood <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(earliest_flood = min(dateF))


