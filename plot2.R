## Name: plot2.R
## Description: Produce a plot to answer the following question
##   Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
##   from 1999 to 2008? Use the base plotting system to make a plot answering this question.
##
## Strategy:
##   Load Data Sets
##   Filter as Needed
##   Group and Aggregate Results
##   Scale as Needed
##   Plot Results

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
yeartotal<-aggregate(datasubset$Emissions, by = list(datasubset$year), FUN="sum")

colnames(yeartotal)<-c("year","total")

yeartotal[,2]<-yeartotal[,2]/1000

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot2.png", width=480, height=480 )

plot(yeartotal, 
     col="blue",
     main="Total Baltimore City, Maryland PM2.5 Emissions",
     ylab="Total (thousands)",
     xlab="Year",
     type="b"
)

abline(glm(yeartotal$total~yeartotal$year),col="red",lwd=1)
legend("topright",c("Actuals", "Overall Trend"), col=c("blue","Red"),lwd=1)

ret <- dev.off()
