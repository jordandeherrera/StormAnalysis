# import data into a data frame
stormdata <- read.csv("repdata-data-StormData.csv.bz2")

# take a look at what the data looks like
head(stormdata)

# turn the column labels into a vector to more easily view them and determine relevant column headings
stormdata.colnames <- colnames(stormdata)

# view the column labels
View(stormdata.colnames)