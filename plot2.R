########################################################################
## plot2.R
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

## Subset only Baltimore City rows
bc <- NEI[NEI$fips=="24510",]

## Find sum of all Emissions grouped by year and make vector
totals <- with(bc, tapply(Emissions, year, sum, na.rm = TRUE))

## Open PNG graphic device
png(filename = "plot2.png", width = 800, height = 600)

## Base graphics barplot
barplot(totals, main = "Total PM2.5 Emissions for Baltimore City, Maryland
        \nwith Linear Fit Model", ylab = "PM2.5 Emissions (Tons)", 
        xlab = "Year", col = "lightblue")

## Create the formula for linear regression model
x <- 1:4; y <- as.vector(totals); fit <- lm(y ~ x)

## Plot the descending line
abline(fit, lty = "dashed")

## Close and save PNG graphic
dev.off()