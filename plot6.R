########################################################################
## plot6.R
## Author: Emilie H. Wolf
## Date: Sunday, May 28, 2017
## Description: This is one of the scripts for the final peer-review
## assignment (AKA Course Project 2) for Coursera's Exploratory Data
## Analysis class.
## 
## Please ensure this R script is in the same folder as the .rds
## files and that your working directory is set to that folder.
########################################################################

## Read in the data
NEI <- readRDS("summarySCC_PM25.rds")

## Subset data for motor vehicle emissions in Baltimore City and LA County
mv <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD", 
             select = c(Emissions,year,fips))

## Find the totals of emissions by both year and location
totals <- aggregate(Emissions ~ year + fips, data = mv, FUN = "sum")

## Get ready to plot
library(ggplot2)
theme_set(theme_bw(base_size = 14))

## Two lines on one graph to show scale
plot6a <- ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "Comparison of Motor Vehicle Emissions") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))

## Subtract the 1999 levels to show change from reference point
totals[1:4,3] <- totals[1:4,3] - totals[1,3]
totals[5:8,3] <- totals[5:8,3] - totals[5,3]

## Two lines starting at 0 to better compare depth of change
plot6b <- ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "Change in PM2.5 Emissions (tons)", x = "Year") +
        labs(title = "Motor Vehicle Emissions Over Time") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))

## Side by side ggplots
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
plot6 <- grid.arrange(plot6a, plot6b, ncol = 2)

## Save
ggsave(file = "plot6.png", plot6, width = 10, height = 4, units = "in")