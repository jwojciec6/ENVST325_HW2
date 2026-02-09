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


##Question 1

#get distinct names of sights
floods %>%
  distinct(names)

#make a subgroup of each locations data
floods_Palmdale <- floods[floods$names == "FISHEATING CREEK AT PALMDALE",]
floods_Zolfo <- floods[floods$names == "PEACE RIVER AT US 17 AT ZOLFO SPRINGS",]
floods_Santa <- floods[floods$names == "SANTA FE RIVER NEAR FORT WHITE",]
floods_Trilby <- floods[floods$names == " WITHLACOOCHEE RIVER AT US 301 AT TRILBY",]

#plot each locations gauge height data
plot(floods_Palmdale$dateF, floods_Palmdale$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)", main =  "FISHEATING CREEK AT PALMDALE")

plot(floods_Zolfo$dateF, floods_Zolfo$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)", main = "PEACE RIVER AT US 17 AT ZOLFO SPRINGS")

plot(floods_Santa$dateF, floods_Santa$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)", main = "SANTA FE RIVER NEAR FORT WHITE")

plot(floods_Trilby$dateF, floods_Trilby$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)", main = " WITHLACOOCHEE RIVER AT US 301 AT TRILBY")

