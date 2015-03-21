## Load SQL libraries
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(tcltk)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
## Create a data frame of total emission per year
emmissionPerYear <- sqldf("select sum(Emissions) as totalEmission,year from NEI group by year")
png("NEIplot1.png")
with(emmissionPerYear,plot(year,totalEmission,main = expression("Total PM"[2.5] * " Emission (1999 - 2008)") ,ylab = expression("Total PM"[2.5] * " Emission (in tons)"),type="n",axes=F))
## Plot the points first
points(emmissionPerYear$year,emmissionPerYear$totalEmission,pch=16,col="Red")
## Draw lines connecting the points
lines(emmissionPerYear$year,emmissionPerYear$totalEmission,col="Blue")
## Add a linear model to clearly see either a decline or ascent
model <- lm(totalEmission~year,emmissionPerYear)
abline(model,lwd=2,col="Blue")
## Draw the y-axis
axis(2)
## Draw the x-axis with year values as 1999,2002,2005 and 2008
axis(side = 1,at = seq(1999, 2008, by = 3))
## Draw the box
box()
dev.off()