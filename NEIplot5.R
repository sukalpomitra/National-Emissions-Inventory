## Load SQL libraries
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(tcltk)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## grep all the rows that have the word Vehicles at the ending
nm <- grep('Vehicles$',SCC$EI.Sector,value=T)
## Convert it into a dataframe so that it can be manipulated via sqldf
nm <- as.data.frame(nm)
## Get only the distinct values
nm <- sqldf("select distinct nm from nm")
##subset SCC and get the ids where the source is Vehicles
SCCVehicles <- sqldf("select SCC.SCC from SCC inner join nm on SCC.'EI.Sector' = nm.nm")
## subset NEI where the SCC corresponds to Vehicle sources
NEIVehicles <- sqldf("select NEI.* from NEI inner join SCCVehicles on NEI.SCC = SCCVehicles.SCC")
## Create a data frame of total emission per year for Baltimore
baltimorePerYear <- sqldf("select sum(Emissions) as totalEmission,year from NEIVehicles where fips = 24510 group by year")
png("NEIplot5.png")
with(baltimorePerYear,plot(year,totalEmission,main = expression("Motor Vehicle Related PM"[2.5] * " Emission in Baltimore (1999 - 2008)"),ylab = expression("Motor Vehicle Related PM"[2.5] * " Emission (in tons)"),type="n",axes=F))
## Plot the points first
points(baltimorePerYear$year,baltimorePerYear$totalEmission,pch=16,col="Red")
## Draw lines connecting the points
lines(baltimorePerYear$year,baltimorePerYear$totalEmission,col="Blue")
## Add a linear model to clearly see either a decline or ascent
model <- lm(totalEmission~year,baltimorePerYear)
abline(model,lwd=2,col="Blue")
## Draw the y-axis
axis(2)
## Draw the x-axis with year values as 1999,2002,2005 and 2008
axis(side = 1,at = seq(1999, 2008, by = 3))
## Draw the box
box()
dev.off()