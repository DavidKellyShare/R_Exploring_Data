## Name: plot5.R
##   How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
##
## Strategy:
##   Load Data Sets
##   Filter as Needed
##   Group and Aggregate Results
##   Scale as Needed
##   Plot Results
##
## Required Librbaries: ggplot2, scales

library("ggplot2")
library("scales")
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
datasubset1<-subset(datamerge[grep("^Mobile", datamerge$EI.Sector), ])
datasubset<-subset(datasubset1[datasubset1$fips == "24510", ])

###################################
# Aggregate and Scale the Data
###################################
print("Aggregating Data")

# Rename existing sectors and calculate Individual Sectors and All Combined
datasubset$EI.Sector<-sub("Mobile - ", "", datasubset$EI.Sector)
datasubset$ALL.Sector<-" All Mobile Sources"
yeartotal1<-aggregate(datasubset$Emissions, by = list(datasubset$year,datasubset$EI.Sector), FUN="sum")
grandtotal<-aggregate(datasubset$Emissions, by = list(datasubset$year,datasubset$ALL.Sector), FUN="sum")

# Concatenate Data Frames
yeartotal<-rbind(yeartotal1,grandtotal)

# Set Column Names and Type
colnames(yeartotal)<-c("Year", "Sector", "Total")
yeartotal$Year<-as.factor(yeartotal$Year)

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot5.png", width=480, height=480 )


print(ggplot(yeartotal, aes(x=Year,
                            y=Total, 
                            group=Sector,
                            color=Sector))
      + ggtitle("Total United States PM2.5 Emissions\nby Mobile Sectors")
      + ylab("Total (thousands)")
      + geom_line() 
      + geom_point()
      #+ theme(legend.position=c(0.5,.5))
      + coord_trans(y="log2",limy=c(1,900))
      #+ coord_cartesian(ylim=c(100,900))
      
)

ret <- dev.off()
