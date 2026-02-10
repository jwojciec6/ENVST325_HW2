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


##Question 2

#find earliest gauge height for each marker level
action_earliest <- floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise(earliest_date_action = min(dateF))

flood_earliest <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(earliest_date_flood = min(dateF))

moderate_earliest <- floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(earliest_date_moderate = min(dateF))

major_earliest <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(earliest_date_major = min(dateF))

#combine all the marker times into one data frame
half1 <- full_join(action_earliest,
                    flood_earliest, 
                    by="names")

half2 <- full_join(moderate_earliest,
                   major_earliest, 
                   by="names") 

final_earliest_times <- full_join(half1, half2, by="names")  


##Question 3 

#create a data frame containing highest gauge level above major category
highest_stream_stage <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(height_above_major = max(max(gheight.ft) - major.ft))

##question 4

##use select to remove excess columns
floods_no_extras <-select(floods, -agency, -siteID)

##use ifelse to make a new category for major flood level or not.

floods_no_extras$major_risk <- ifelse(floods_no_extras$gheight.ft >= floods_no_extras$major.ft, 
                                      "yes","no" )

##Create a histogram of the frequencies of each gauge depth for Santa Fe location

hist(floods_Santa$gheight.ft, main = "Frequeincies of Depths Santa Fe",
     xlab = "Depth (ft)")


  
