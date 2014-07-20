## Name: plot6.R
##   Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
##   vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen 
##   greater changes over time in motor vehicle emissions?
##
## Strategy:
##   Load Data Sets
##   Filter as Needed
##   Group and Aggregate Results
##   Scale as Needed
##   Plot Results
##

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
datasubset1<-subset(datamerge[datamerge$SCC.Level.One == "Mobile Sources", ])
datasubset<-subset(datasubset1[datasubset1$fips %in% c("24510","06037"), ])

###################################
# Aggregate and Scale the Data
###################################
print("Aggregating Data")
yeartotal<-aggregate(datasubset$Emissions, by = list(datasubset$year,datasubset$fips), FUN="sum")

colnames(yeartotal)<-c("Year", "City", "Total")

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot6.png", width=480, height=480 )

# Subset Baltimore and Scale SUM between 0 & 1
yearslice <- yeartotal[yeartotal$City %in% c("24510"), ]
yearslice[,3] <- (yearslice[,3] - min(yearslice[,3])) / (max(yearslice[,3])-min(yearslice[,3]))

# Increase Margin for Double Line Y Axis Title
par(mar=c(5,5,4,2))
plot(yearslice$Year, yearslice$Total, 
     col="black",
     main="PM2.5 Motor Vehicle Emissions",
     type="b",
     lty="solid",
     xlab="Year",
     ylab="Relative Total Emissions\n(Seperatly Scaled between 0-1)"
)

abline(glm(yearslice$Total~yearslice$Year),col="green",lwd=1)


# Subset Los Angeles and Scale SUM between 0 & 1
yearslice <- yeartotal[yeartotal$City %in% c("06037"), ]
yearslice[,3] <- (yearslice[,3] - min(yearslice[,3])) / (max(yearslice[,3])-min(yearslice[,3]))

lines(yearslice$Year, yearslice$Total, col="red", type="b", lty="dotted")

abline(glm(yearslice$Total~yearslice$Year),col="blue",lwd=1)

legend("topright",
       c("Baltimore City, Maryland (Detail)",
         "Baltimore City, Maryland (Trend)",
         "Los Angeles County (Detail)",
         "Los Angeles County (Trend)"),
       col=c("black","green","red","blue"),
       lwd=1)

ret <- dev.off()
