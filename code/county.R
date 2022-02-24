###############
#Data Cleaning#
###############
#install.packages('tidyverse')
#install.packages('readstata13')
library(tidyverse)
library(readstata13)


setwd("D:/Documents/School/Baylor/Example")

county <- data.frame(read.dta13("data/Lott_Dataset.dta"))
write.csv(county, "D:\\Documents\\School\\Baylor\\example\\data\\rawLottCounty.csv", row.names=FALSE)
#Sort Data
county <- county[order(county$fipsid,county$year),]

#Remove Always Treated
county <- county[!county$yshall==1,]

#Remove years beyond the bounds of the study
county <- county[county$year>=1977 & county$year<=2000,]

#Remove any counties with entirely missing entries
county <- county %>%
  group_by(fipsid) %>%
  filter(!all(is.na(fipsstat)))

#Remove duplicate entries
county <- county %>%
  distinct()

#Remove any counties that reported zero crimes
county <- county %>%
  group_by(fipsid) %>%
  filter(!all(is.na(mur)))

#Assign treatment dates
county$treat_date <- 0
sum(is.na(county$fipsstat))
county <- county[!is.na(county$fipsid),]
for (i in 1:nrow(county)) {
  if (county$shallf[i]==0 & county$shallf[i+1]==1){
    county$treat_date[i] <- county$year[i]
  }
}

county <- county[order(county$fipsid,county$year),]
for (i in 1:nrow(county)) {
  if (i==nrow(county)){break}
  if (county$treat_date[i]!=0 & county$treat_date[i+1]==0 & county$fipsstat[i]==county$fipsstat[i+1]){
    county$treat_date[i+1] <- county$treat_date[i]
  }
}

county <- county[order(-county$fipsid,-county$year),]
for (i in 1:nrow(county)) {
  if (i==nrow(county)){break}
  if (county$treat_date[i]!=0 & county$treat_date[i+1]==0 & county$fipsstat[i]==county$fipsstat[i+1]){
    county$treat_date[i+1] <- county$treat_date[i]
  }
}
county <- county[order(county$fipsid,county$year),]


#Save Dataset
write.csv(county, "D:\\Documents\\School\\Baylor\\example\\data\\county_cleaned.csv", row.names=FALSE)

###################
#Table Replication#
###################
#Reset cleaning to the amount done in the study
county <- data.frame(read.dta13("data/Lott_Dataset.dta"))
county <- county[county$year>=1977 & county$year<=2000,]

#Perform TWFE regression
#install.packages('fixest')
library(fixest)
mur <- feglm(lratmur ~ shallf|fipsid + year, weights = county$popc, data=county)
#mur <- feglm(lratmur ~ shallf|fipsid + year, weights = county$popc, cluster=fipsstat, data=county)

#Repeat for the other crime categories
rap <- feglm(lratrap ~ shallf|fipsid + year, weights = county$popc, data=county)
aga <- feglm(lrataga ~ shallf|fipsid + year, weights = county$popc, data=county)
rob <- feglm(lratrob ~ shallf|fipsid + year, weights = county$popc, data=county)
aut <- feglm(lrataut ~ shallf|fipsid + year, weights = county$popc, data=county)
bur <- feglm(lratbur ~ shallf|fipsid + year, weights = county$popc, data=county)
lar <- feglm(lratlar ~ shallf|fipsid + year, weights = county$popc, data=county)

#Prepare columns of tables
ATTmur <- round(100*mur[["coefficients"]][["shallf"]], 2)
SEmur <- round(100*mur[["se"]][["shallf"]], 2)

ATTrap <- round(100*rap[["coefficients"]][["shallf"]], 2)
SErap <- round(100*rap[["se"]][["shallf"]], 2)

ATTaga <- round(100*aga[["coefficients"]][["shallf"]], 2)
SEaga <- round(100*aga[["se"]][["shallf"]], 2)

ATTrob <- round(100*rob[["coefficients"]][["shallf"]], 2)
SErob <- round(100*rob[["se"]][["shallf"]], 2)

ATTaut <- round(100*aut[["coefficients"]][["shallf"]], 2)
SEaut <- round(100*aut[["se"]][["shallf"]], 2)

ATTbur <- round(100*bur[["coefficients"]][["shallf"]], 2)
SEbur <- round(100*bur[["se"]][["shallf"]], 2)

ATTlar <- round(100*lar[["coefficients"]][["shallf"]], 2)
SElar <- round(100*lar[["se"]][["shallf"]], 2)

#Set up table
part1 <- as.data.frame(cbind(ATTmur, ATTrap, ATTaga, ATTrob, ATTaut, ATTbur, ATTlar))
colnames(part1) <- c("Murder", "Rape", "Assault", "Robbery", "Auto", "Burglary", "Larceny")
part2 <- as.data.frame(cbind(SEmur, SErap, SEaga, SErob, SEaut, SEbur, SElar))
colnames(part2) <- c("Murder", "Rape", "Assault", "Robbery", "Auto", "Burglary", "Larceny")
countytable <- rbind(part1, part2)
rownames(countytable) <- c("ATT", "Standard Error")
countytable

#Save Table
#write.csv(countytable, "D:\\Documents\\School\\Baylor\\tables\\countytable.csv", quote = F)

###############
#Figure Design#
###############
#install.packages('ggplot2')
library(ggplot2)

#Organize data into adopter categories
cleanedcounty <- read.csv("data/county_cleaned.csv")
early <- cleanedcounty[cleanedcounty$treat_date >= 1985 & cleanedcounty$treat_date <= 1988,]
mid <- cleanedcounty[cleanedcounty$treat_date >= 1989 & cleanedcounty$treat_date <= 1991,]
late <- cleanedcounty[cleanedcounty$treat_date >= 1994 & cleanedcounty$treat_date <= 1996,]
nonrtc <- cleanedcounty[cleanedcounty$treat_date == 0,]

#condense to relevant variables and remove NAs
early <- na.omit(early[,c("year","fipsid","fipsstat","lratmur","treat_date","popc")])
mid <- na.omit(mid[,c("year","fipsid","fipsstat","lratmur","treat_date","popc")])
late <- na.omit(late[,c("year","fipsid","fipsstat","lratmur","treat_date","popc")])
nonrtc <- na.omit(nonrtc[,c("year","fipsid","fipsstat","lratmur","treat_date","popc")])

#Create weighted mean by year
early <- early %>%
  group_by(year) %>%
  mutate(weighted_mur = weighted.mean(lratmur, popc))

weightedearly <- early[,c("year","weighted_mur")]
weightedearly <- weightedearly %>%
  distinct()
weightedearly <- weightedearly[order(weightedearly$year),]

mid <- mid %>%
  group_by(year) %>%
  mutate(weighted_mur = weighted.mean(lratmur, popc))

weightedmid <- mid[,c("year","weighted_mur")]
weightedmid <- weightedmid %>%
  distinct()
weightedmid <- weightedmid[order(weightedmid$year),]

late <- late %>%
  group_by(year) %>%
  mutate(weighted_mur = weighted.mean(lratmur, popc))

weightedlate <- late[,c("year","weighted_mur")]
weightedlate <- weightedlate %>%
  distinct()
weightedlate <- weightedlate[order(weightedlate$year),]

nonrtc <- nonrtc %>%
  group_by(year) %>%
  mutate(weighted_mur = weighted.mean(lratmur, popc))

weightednonrtc <- nonrtc[,c("year","weighted_mur")]
weightednonrtc <- weightednonrtc %>%
  distinct()
weightednonrtc <- weightednonrtc[order(weightednonrtc$year),]

#Combine into one dataset
weightedearly$group <- "Early Adopters"
weightedmid$group <- "Mid Adopters"
weightedlate$group <- "Late Adopters"
weightednonrtc$group <- "Non-RTC States"
weightedmur <- rbind(weightedearly, weightedmid, weightedlate, weightednonrtc)

weightedmur %>%
  ggplot (aes(x=year, y=weighted_mur,group=group,color=group)) + geom_line() + ylab("Weighted Murder Rate")
