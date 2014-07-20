## Name: plot4.R
##   Across the United States, how have emissions from coal combustion-related sources 
##   changed from 1999–2008?
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
datasubset<-subset(datamerge[grep("Coal",datamerge$EI.Sector), ])

###################################
# Aggregate, Rename and Scale the Data
###################################
print("Aggregating Data")

# Calculate Individual Sectors and All Combined
datasubset$ALL.Sector<-"All Coal Sectors"
yeartotal1<-aggregate(datasubset$Emissions, by = list(datasubset$year,datasubset$EI.Sector), FUN="sum")
grandtotal<-aggregate(datasubset$Emissions, by = list(datasubset$year,datasubset$ALL.Sector), FUN="sum")

# Concatenate Data Frames
yeartotal<-rbind(yeartotal1,grandtotal)

# Set Column Names and Type
colnames(yeartotal)<-c("Year", "Sector", "Total")
yeartotal$Year<-as.factor(yeartotal$Year)

# Rename Sector for Plotting
levels(yeartotal$Sector)[levels(yeartotal$Sector)=="Fuel Comb - Comm/Institutional - Coal"] <- "Comm/Institutional"
levels(yeartotal$Sector)[levels(yeartotal$Sector)=="Fuel Comb - Electric Generation - Coal"] <- "Electric Generation"
levels(yeartotal$Sector)[levels(yeartotal$Sector)=="Fuel Comb - Industrial Boilers, ICEs - Coal"] <- "Industrial Boilers, ICEs"
names(yeartotal)[names(yeartotal)=="Sector"] <- "Coal Sector"

# Scale Y Axis
yeartotal[,3]<-yeartotal[,3]/1000

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot4.png", width=480, height=480 )

print(ggplot(yeartotal, aes(x=Year,
                            y=Total, 
                            group=`Coal Sector`,
                            color=`Coal Sector`))
      + ggtitle("Total United States PM2.5 Emissions\nby Coal Sector")
      + ylab("Total (thousands)")
      + geom_line() 
      + geom_point()
      + theme(legend.position=c(0.5,.5))
      )

ret <- dev.off()
