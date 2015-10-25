# import data into a data frame
#stormdata <- read.csv("repdata-data-StormData.csv.bz2")

# take a look at what the data looks like
head(stormdata)

# turn the column labels into a vector to more easily view them and determine relevant column headings
stormdata.colnames <- colnames(stormdata)

# view the column labels
View(stormdata.colnames)

# create a new data frame that has only relevant columns for population health
# remove na values
stormdata.pop.health <- na.omit(stormdata[,c(8,23,24)])

# create a new data frame that has only relevant columns for population health
# remove na values
stormdata.economic <- na.omit(stormdata[,c(8,25:28)])

# convert to upper case
for (i in c(3,5))
{
stormdata.economic.multiplier[,i] <- toupper(stormdata.economic.multiplier[,i])
}

# create property damage data frame
stormdata.economic.multiplier.property <- stormdata.economic[stormdata.economic[,3] %in% c("K","M","B"),1:3]

# create crop damage data frame
stormdata.economic.multiplier.crop <- stormdata.economic[stormdata.economic[,5] %in% c("K","M","B"),c(1,4:5)]

# create multiplier data frame
multiplier.df <- data.frame(Alpha = as.factor(c("K","M","B")), Multiplier = c(1000,1000000,1000000000))

# merge to create property and crop data frames
stormdata.property <- merge(stormdata.economic.multiplier.property, multiplier.df, by.x = "PROPDMGEXP", by.y = "Alpha")
stormdata.property <- data.frame(Event = stormdata.property$EVTYPE, Property = stormdata.property$PROPDMG * stormdata.property$Multiplier)

stormdata.crop <- merge(stormdata.economic.multiplier.crop, multiplier.df, by.x = "CROPDMGEXP", by.y = "Alpha")
stormdata.crop <- data.frame(Event = stormdata.crop$EVTYPE, Crop = stormdata.crop$CROPDMG * stormdata.crop$Multiplier)

# load dplyr library for data manipulation
library(dplyr)

#create a group according to event type to calculate mean and sum
by_event <- group_by(stormdata.pop.health, EVTYPE)

#summarize data and put into a data frame, round to nearest whole number for better graphing
mean.fatalities <- data.frame(summarise(by_event, round(mean(FATALITIES),0)))

#add column names for clarity
colnames(mean.fatalities) <- c("Event","Mean.Fatalities")

#summarize data and put into a data frame, round to nearest whole number for better graphing
sum.fatalities <- data.frame(summarise(by_event, round(sum(FATALITIES),0)))

#add column names for clarity
colnames(sum.fatalities) <- c("Event","Sum.Fatalities")

#summarize data and put into a data frame, round to nearest whole number for better graphing
mean.injuries <- data.frame(summarise(by_event, round(mean(INJURIES),0)))

#add column names for clarity
colnames(mean.injuries) <- c("Event","Mean.Injuries")

#summarize data and put into a data frame, round to nearest whole number for better graphing
sum.injuries <- data.frame(summarise(by_event, round(sum(INJURIES),0)))

#add column names for clarity
colnames(sum.injuries) <- c("Event","Sum.Injuries")

#create a group according to event type 
by_event <- group_by(stormdata.crop, Event)

#summarize data and put into a data frame, round to nearest whole number for better graphing
mean.crop <- data.frame(summarise(by_event, round(mean(Crop),0)))

#add column names for clarity
colnames(mean.crop) <- c("Event","Mean.Crop")

#create a group according to event type 
by_event <- group_by(stormdata.property, Event)

#summarize data and put into a data frame, round to nearest whole number for better graphing
mean.property <- data.frame(summarise(by_event, round(mean(Property),0)))

#add column names for clarity
colnames(mean.property) <- c("Event","Mean.Property")

#load ggplot2 library for plotting
library(ggplot2)

#plot a histogram of means
bar.mean.fatalities <- qplot(y = Mean.Fatalities, x = Event,
                        data=mean.fatalities[mean.fatalities$Mean.Fatalities>5,], geom="bar", fill=Event, stat="identity") +  
  labs(title="Mean Fatalities by Event Type") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a histogram of sum totals by event type
bar.sum.fatalities <- qplot(y = Sum.Fatalities, x = Event, 
                              data=sum.fatalities[sum.fatalities$Sum.Fatalities>500,], geom="bar", fill=Event, stat="identity") +  
  labs(title="Total Fatalities by Event Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a histogram of means
bar.mean.injuries <- qplot(y = Mean.Injuries, x = Event, 
                              data=mean.injuries[mean.injuries$Mean.Injuries>10,], geom="bar", fill=Event, stat="identity") +  
  labs(title="Mean Injuries by Event Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a histogram of sum totals by event type
bar.sum.injuries <- qplot(y = Sum.Injuries, x = Event, 
                             data=sum.injuries[sum.injuries$Sum.Injuries>5000,], geom="bar", fill=Event, stat="identity") +  
  labs(title="Total Injuries by Event Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a boxplot of injuries by event type
box.injuries <- qplot(x = EVTYPE, y = INJURIES, 
                           data=stormdata.pop.health[stormdata.pop.health$INJURIES>10,], geom="boxplot", fill=EVTYPE) +  
  labs(title="Property Damage by Event Type", y="Injuries", x="Event") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a histogram of sum totals by event type
hist.mean.property <- qplot(Mean.Property, 
                           data=mean.property[mean.property$Mean.Property>1,], geom="histogram", fill=Event) +  
  labs(title="Mean Property Damage by Event Type")

#plot a boxplot of all property damage by event type
box.property <- qplot(x = Event, y = Property, 
                            data=stormdata.property[stormdata.property$Property>900000000,], geom="boxplot", fill=Event) +  
  labs(title="Property Damage by Event Type", y="Property Damage in $") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#plot a histogram of sum totals by event type
hist.mean.crop <- qplot(Mean.Crop, 
                           data=mean.crop[mean.crop$Mean.Crop>1,], geom="histogram", fill=Event) +  
  labs(title="Mean Crop Damage by Event Type")

#plot a boxplot of all crop damage by event type
box.crop <- qplot(x = Event, y = Crop, 
                      data=stormdata.crop[stormdata.crop$Crop>45000000,], geom="boxplot", fill=Event) +  
  labs(title="Crop Damage by Event Type", y="Crop Damage in $") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#load grid package for multiplot
library(grid)

#function that creates multiple plots in one figure
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#create multiple plots
plot.health <- multiplot(bar.mean.fatalities, bar.sum.fatalities, bar.mean.injuries,
                         bar.sum.injuries, box.property, box.crop, cols=3)