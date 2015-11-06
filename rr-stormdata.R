library(dplyr)
library(readr)

###################################################################################
# Data Preprocessing
storm_data <- read_csv("repdata-data-StormData.csv")
data <- storm_data
# tidy up variable names for easier calling and manipulation
names(data) <- tolower(names(data))

# Grab only the needed columns for faster manipulation
data <- data %>%
  select(bgn_date, evtype, fatalities, injuries, propdmg, propdmgexp, cropdmg,
         cropdmgexp)
#format date; end_dates are full of NA values
data$bgn_date <- as.Date(data$bgn_date, "%m/%d/%Y")

post_1996 <- data %>%
  filter(bgn_date >= "1996-01-01")

# Look at the number of events by year, disregarding type for now
library(lubridate)
post_1996$year <- year(post_1996$bgn_date) 

library(ggplot2)
qplot(post_1996$year)

# So it definitely appears that data collections has increased over the years since 1996,
# but whether this is significant remains to be seen

# Cleaning evtype before looking at it
post_1996$evtype <- tolower(post_1996$evtype)



types <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill",
           "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm",
           "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze",
           "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow",
           "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow",
           "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind",
           "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", 
           "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
           "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
           "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
types <- tolower(types)

post_1996$is_48 <- ifelse(post_1996$evtype %in% types, TRUE, FAxLSE)
is_48 <- post_1996 %>%
  filter(is_48 == TRUE)
is_not_48 <- post_1996 %>%
  filter(is_48 == FALSE)

# Ordering evtype by number of deaths
by_deaths <- post_1996 %>%
  group_by(evtype) %>%
  summarise(deaths = sum(fatalities)) %>%
  arrange(desc(deaths))

by_injuries <- post_1996 %>%
  group_by(evtype) %>%
  summarise(injuries = sum(injuries)) %>%
  arrange(desc(injuries))
             