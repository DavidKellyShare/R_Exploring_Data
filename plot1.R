## Name: plot1.R
## Description: Produce a plot to answer the following question
##   Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##   Using the base plotting system, make a plot showing the total PM2.5 emission from 
##   all sources for each of the years 1999, 2002, 2005, and 2008.
##
## Strategy:
##   Load Data Sets
##   Filter as Needed
##   Group and Aggregate Results
##   Scale as Needed
##   Plot Results

###################################
# Source the Data
###################################
print("Unzipping exdata_data_NEI_data.zip")
unzip("exdata_data_NEI_data.zip") 

print("Loading summarySCC_PM25.rds")
NEI <- readRDS("summarySCC_PM25.rds")

print("Loading Source_Classification_Code.rds")
SCC <- readRDS("Source_Classification_Code.rds")

print("Merging Data on SCC")
datamerge<-merge(NEI,SCC,by.x="SCC", by.y="SCC")

###################################
# Aggregate and Scale the Data
###################################
print("Aggregating Data")
yeartotal<-aggregate(datamerge$Emissions, by = list(datamerge$year), FUN="sum")

colnames(yeartotal)<-c("year","total")

yeartotal[,2]<-yeartotal[,2]/1000000

###################################
# Plot the Data
###################################
print("Plotting Data")
dev <- png(filename="plot1.png", width=480, height=480 )

plot(yeartotal, 
     col="blue",
     main="Total United States PM2.5 Emissions",
     ylab="Total (millions)",
     xlab="Year",
     type="b"
     )

abline(glm(yeartotal$total~yeartotal$year),col="red",lwd=1)
legend("topright",c("Actuals","Overall Trend"), col=c("blue", "Red"),lwd=1)

ret <- dev.off()
