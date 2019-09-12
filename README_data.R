setwd('~/Visualizing-Butterfly-Emergence')
allData <- read.csv(file="Output.ALL.PresenceAbsence.RCP8.5.csv")

#get years
emerge <- as.data.frame(unique(allData$Year))
names(emerge)[1] <- "Year"

for (i in 3:22){
  #remove all absence data for each column of interest
  presence <- allData[!(allData[i] == "1:ABSENT"),] 
  ###extracting earliest emergence dates
  calc <- presence %>%
    group_by(Year) %>%
    summarise(early = min(Ordinal))
  emerge <- cbind(emerge,calc$early)
}

View(emerge)