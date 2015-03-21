## Load SQL libraries
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(tcltk)
library(ggplot2)
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
## Create a data frame of total emission per year for Baltimore and LA
baltimoreLAPerYear <- sqldf("select sum(Emissions) as totalEmission,year,fips from NEIVehicles where fips IN ('24510','06037') group by year,fips")
## Convert the fips column as factor variable
baltimoreLAPerYear[,3] <- as.numeric(baltimoreLAPerYear[,3])
baltimoreLAPerYear[,3] <- factor(baltimoreLAPerYear[,3],levels=c(06037,24510), labels=(c("Los Angeles County","Baltimore City")))
png("NEIplot6.png", width=800,height=600)
## plot the aesthetic mapping
g <- ggplot(baltimoreLAPerYear,aes(year,totalEmission))
## Add points and a linear model
p <- g + geom_point(aes(color=fips)) + facet_grid(.~fips) + geom_smooth(method="lm",se=F,aes(color=fips)) + labs(title = expression("Motor  Vehicle Related " * PM[2.5] * " Emissions in Baltimore & Los Angeles (1999 - 2008)")) + labs(y=expression("Motor  Vehicle Related " * PM[2.5] * " Emissions (in tons)")) + theme(legend.position="none", axis.text = element_text(size = 8)) + scale_x_continuous(breaks = seq(1999, 2008, 3))
print(p)
dev.off()