########################################################################
## plot1.R
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

## Find sum of all Emissions grouped by year and make vector
totals <- with(NEI, tapply(Emissions, year, sum, na.rm = T))

## Open PNG graphic device
png(filename = "plot1.png", width = 800, height = 600)

## Simple base barplot of Emissions, tons converted to teragrams
par(mfrow=c(1,1))
barplot(totals*.00000090718474, main = "Total PM2.5 Emissions From All Sources",
        ylab = "Teragrams of PM2.5", xlab = "Year", col = "tan")

## Close and save PNG graphic
dev.off()