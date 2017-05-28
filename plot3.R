########################################################################
## plot3.R
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

## Subset out only rows in Baltimore City
bc <- subset(NEI, fips == "24510", c(Emissions,type,year))

## Get the sums grouped by year and type and create data frame
totals <- aggregate(Emissions ~ year + type, data = bc, FUN = "sum")

## Get ready to plot
library(ggplot2)
theme_set(theme_bw(base_size = 14))

## A nice colorful line graph
plot3 <- ggplot(totals, aes(year,Emissions,color=type)) + geom_line() + 
        geom_point(aes(shape = type), size = 3) +
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD \nby Source Type")

## Save
ggsave(file = "plot3.png", plot3, width = 6.4, height = 4.8, units = "in")