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
datasubset<-subset(datasubset1[datasubset1$fips == "24510", ])

###################################
# Aggregate and Scale the Data
###################################
print("Aggregating Data")
yeartotal<-aggregate(datasubset$Emissions, by = list(datasubset$year), FUN="sum")

colnames(yeartotal)<-c("Year", "Total")

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot5.png", width=480, height=480 )

plot(yeartotal, 
     col="blue",
     main="Baltimore City, Maryland PM2.5 Moter Vehicle Emissions",
     type="b"
)

abline(glm(yeartotal$Total~yeartotal$Year),col="red",lwd=1)
legend("topright","Overall Trend", col="Red",lwd=1)

ret <- dev.off()
