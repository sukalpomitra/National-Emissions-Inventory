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
## Convert type to factor
NEI[,5] <- as.factor(NEI[,5])
## Create a data frame of total emission per year per type for Baltimore
baltimorePointPerYear <- sqldf("select sum(Emissions) as totalEmission,year,type from NEI where fips = 24510 and type = 'POINT' group by year")
baltimoreNonPointPerYear <- sqldf("select sum(Emissions) as totalEmission,year,type from NEI where fips = 24510 and type = 'NONPOINT' group by year")
baltimoreOnRoadPerYear <- sqldf("select sum(Emissions) as totalEmission,year,type from NEI where fips = 24510 and type = 'ON-ROAD' group by year")
baltimoreNonRoadPerYear <- sqldf("select sum(Emissions) as totalEmission,year,type from NEI where fips = 24510 and type = 'NON-ROAD' group by year")
## Combine the data as a single data frame
baltimoreEmissionTypePerYear <- rbind(baltimorePointPerYear,baltimoreNonPointPerYear,baltimoreOnRoadPerYear,baltimoreNonRoadPerYear)
png("NEIplot3.png", width=800,height=600)
## plot the aesthetic mapping
g <- ggplot(baltimoreEmissionTypePerYear,aes(year,totalEmission))
## Add points and a linear model
p <- g + geom_point(aes(color=type)) + facet_grid(.~type) + geom_smooth(method="lm",se=F,aes(color=type))+ labs(title = expression("Total " * PM[2.5] * " Emissions in Baltimore (1999 - 2008)")) + labs(y=expression("Total PM"[2.5] * " Emission (in tons)")) + theme(legend.position="none", axis.text = element_text(size = 8)) + scale_x_continuous(breaks = seq(1999, 2008, 3))
print(p)
dev.off()