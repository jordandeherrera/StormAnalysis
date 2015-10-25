# import data into a data frame
stormdata <- read.csv("repdata-data-StormData.csv.bz2")

# take a look at what the data looks like
head(stormdata)

# turn the column labels into a vector to more easily view them and determine relevant column headings
stormdata.colnames <- colnames(stormdata)

# view the column labels
View(stormdata.colnames)

# create a new data frame that has only relevant columns for population heath
# remove na values
stormdata.pop.health <- na.omit(stormdata[,c(8,23,24)])

# omit zeroes
stormdata.pop.health <- stormdata.pop.health[stormdata.pop.health$FATALITIES > 5,]

# load dplyr library for data manipulation
library(dplyr)

#create a group according to event type to calculate mean and median
by_event <- group_by(stormdata.pop.health, EVTYPE)

#summarize data and put into a data frame, round to nearest whole number for better graphing
mean.fatalities <- data.frame(summarise(by_event, round(mean(FATALITIES),0)))

#add column names for clarity
colnames(mean.fatalities) <- c("Event","Mean.Fatalities")

#load ggplot2 library for plotting
library(ggplot2)

#plot a histogram of means
hist.mean.fatalities <- qplot(Mean.Fatalities, 
                        data=mean.fatalities, geom="histogram", fill=Event) +  
  labs(title="Histogram of Mean Fatalities by Event Type")

#summarize data and put into a data frame, round to nearest whole number for better graphing
sum.fatalities <- data.frame(summarise(by_event, round(sum(FATALITIES),0)))

#add column names for clarity
colnames(sum.fatalities) <- c("Event","Sum.Fatalities")

#plot a histogram of sum totals by event type
hist.sum.fatalities <- qplot(Sum.Fatalities, 
                              data=sum.fatalities, geom="histogram", fill=Event) +  
  labs(title="Histogram of Total Fatalities by Event Type")
