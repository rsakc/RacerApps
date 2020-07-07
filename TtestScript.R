## T-test Script
library(shiny)
#library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/racer/getdata.php") 

data.all <- filter(data.all, Body == "Bayes" | Body == "Nightingale" | Body == "Gauss")
data.all <- filter(data.all, Level == "Tutorial" | Level == "Paired")
data.all <- filter(data.all, Track == "Tutorial" | Track == "StraightTrack" | Track == "OvalTrack" | Track == "8Track" | Track == "ComplexTrack" )
data.all <- droplevels(data.all)

## 2020 Data Only (Note there was one bad game in Jan so we start on Feb 1)
## This should now work for any date:
data.all <- data.all %>% mutate(Date = str_sub(GameDate, 1, 10))
data.all$Date <- as.Date(data.all$Date, format = "%m/%d/%Y")
data.all <- data.all %>% filter(Date >= as.Date("02/01/2020", format = "%m/%d/%Y"))

data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

## These were used to verify the code below worked:
#data.all <- filter(data.all, GroupID == "stat5140sp20" | GroupID == "test" | GroupID == "stest"| GroupID == "mtsu3340")
#data.all <- filter(data.all, GroupID == "stat5140sp20")

#Sort Data in order to create an accurate Order column
data.all <- data.all %>% arrange(Level, Track, GroupID, PlayerID, Game)

#Make a working Order column called Order2
data.all$Order2 <- NA
data.all$Order2[1] <- 1 
for(i in 2:nrow(data.all)){
  if(data.all$PlayerID[i] == data.all$PlayerID[i - 1] &
     data.all$GroupID[i] == data.all$GroupID[i - 1] &
     data.all$Track[i] == data.all$Track[i - 1] &
     data.all$Level[i] == data.all$Level[i - 1]){
    
    data.all$Order2[i] <- data.all$Order2[i - 1] + 1
    } 
    
  else {
    data.all$Order2[i] <- 1
  }
}

## We only keept the first two clean races
data.all <- filter(data.all, Order2 < 3)

## Don't keep if they only played one race
data.all$Clean <- "Yes"
for(i in 1:nrow(data.all)){
  if(data.all$Order2[i] == 1 & data.all$Order2[i+1] == 1){
    data.all$Clean[i] <- "No"
    }
}

# Don't keep if they played the same car twice
for(i in 2:nrow(data.all)){
  if(data.all$PlayerID[i] == data.all$PlayerID[i - 1] &
    data.all$Car[i] == data.all$Car[i - 1]){
    data.all$Clean[i - 1] <- "No" 
    data.all$Clean[i] <- "No"
  }
}

## We need a checkbox (Use "Only Clean Data)
data.clean = filter(data.all, Clean == Yes)

## ONLY if the data is clean, we can then filter to eliminate BadDrivers
## If the "Only Clean Data" is checked, then we can also checkbox "Only Good Drivers"

## We need to take the track into account for bad drivers.
data.clean$BadDriver <- "No"
for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "OvalTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 60 |
        data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 3)){
    data.clean$BadDriver[i - 1] <- "Yes" 
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "Tutorial" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 50 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1)){
    data.clean$BadDriver[i - 1] <- "Yes" 
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "StraightTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 40 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1)){
    data.clean$BadDriver[i - 1] <- "Yes" 
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if((data.clean$Track[i] == "8Track" | data.clean$Track[i] == "ComplexTrack") &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1){
    data.clean$BadDriver[i - 1] <- "Yes" 
    data.clean$BadDriver[i] <- "Yes"
  }
}

##Checkbox for "Only Good Drivers"
data.good = filter(data.clean, BadDriver == "No")

#table(data.clean$PlayerID, data.clean$Order2)
#data.temp <- select(data.clean, Level, Track, GroupID, PlayerID, Order, Order2, Car, Game, Clean,BadDriver, FinishTime, TimeOffTrack)
#View(data.temp)

#table(data.clean$PlayerID, data.clean$Order2)



