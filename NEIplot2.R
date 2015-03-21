## Load SQL libraries
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(tcltk)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
## Create a data frame of total emission per year for Baltimore
baltimorePerYear <- sqldf("select sum(Emissions) as totalEmission,year from NEI where fips = 24510 group by year")
png("NEIplot2.png")
with(baltimorePerYear,plot(year,totalEmission,main = expression("Total PM"[2.5] * " Emission in Baltimore (1999 - 2008)") ,ylab = expression("Total PM"[2.5] * " Emission (in tons)"),type="n",axes=F))
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