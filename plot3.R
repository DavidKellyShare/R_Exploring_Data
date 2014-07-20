## Name: plot3.R
## Description: Produce a plot to answer the following question
##   Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
##   variable, which of these four sources have seen decreases in emissions from 1999–2008 
##   for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the
##   ggplot2 plotting system to make a plot answer this question.
##
## Strategy:
##   Load Data Sets
##   Filter as Needed
##   Group and Aggregate Results
##   Scale as Needed
##   Plot Results
##
## Required Librbary: ggplot2

library("ggplot2")

###################################
# Source and Filter the Data
###################################
print("Unzipping exdata_data_NEI_data.zip")
unzip("exdata_data_NEI_data.zip") 

print("Loading summarySCC_PM25.rds")
NEI <- readRDS("summarySCC_PM25.rds")

print("Loading Source_Classification_Code.rds")
SCC <- readRDS("Source_Classification_Code.rds")

print("Merging Data on SCC")
datamerge<-merge(NEI,SCC,by.x="SCC", by.y="SCC")

print("Filtering Data")
datasubset<-subset(datamerge[datamerge$fips == "24510", ])

###################################
# Aggregate and Scale the Data
###################################
print("Aggregating Data")
yeartotal<-aggregate(datasubset$Emissions, by = list(datasubset$year, datasubset$type), FUN="sum")

colnames(yeartotal)<-c("Year", "Type", "Total")
yeartotal$Year<-as.factor(yeartotal$Year)
yeartotal$Type<-as.factor(yeartotal$Type)

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot3.png", width=480, height=480 )

print(ggplot(yeartotal, aes(x=Year,
                            y=Total, 
                            group=Type,
                            color=Type))
                            + ggtitle("Baltimore City, Maryland PM2.5 Emissions by Type")
                            + geom_line() + geom_point())

ret <- dev.off()
