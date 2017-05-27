## pm25analysis.R
## Author: Emilie H. Wolf
## Date: Friday, May 26, 2017
## Description: This is the final peer-review assignment, (AKA Course Project 2)
## for Coursera's Exploratory Data Analysis class.
## URL of Data: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

## Please ensure this R script is in the same folder as the .rds files and
## that your working directory is set to that folder.

## Before starting my analysis, 
## I downloaded and unzipped the datasets and moved them to a new folder called
## EDA_Wk4Assignment. Next I opened SourceTree and created a new repository.
## Then I opened RStudio and started a new R Project for that folder.
## Now as I work on writing the script, I can save my progess to my GitHub.

##############################################################################

## Read in datasets
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

###########################################
## 1. Have total emissions from PM2.5 decreased in the United States from 1999
## to 2008? Using the base plotting system, make a plot showing the total PM2.5
## emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## Find sum of all Emissions grouped by year and make vector
totals <- with(NEI, tapply(Emissions, year, sum, na.rm = T))

## Simple base barplot of Emissions, tons converted to teragrams
barplot(totals*.00000090718474, main = "Total PM2.5 Emissions From All Sources",
        ylab = "Teragrams of PM2.5", xlab = "Year", col = "tan")

###########################################
## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
## (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system
## to make a plot answering this question.

bc <- subset(NEI, fips == "24510")
totals <- with(bc, tapply(Emissions, year, sum, na.rm=T))
barplot(totals, main = "Total PM2.5 Emissions for Baltimore City, Maryland \nwith Linear Fit Model", 
        ylab = "PM2.5 Emissions (Tons)", xlab = "Year", col = "lightblue")
x <- 1:4
y <- as.vector(totals)
fit <- lm(y ~ x)
abline(fit, lty = "dashed")
## abline(h=y[1],lty = "dotted")

###########################################
## 3. Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint,
## onroad, nonroad) variable, which of these four sources have seen decreases
## in emissions from 1999–2008 for Baltimore City? Which have seen increases
## in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
## to answer this question.

library(ggplot2)
theme_set(theme_bw(base_size = 14))
bc <- subset(NEI, fips == "24510", c(Emissions,type,year))

ggplot(bc, aes(year, Emissions, color = type)) +
        geom_line(stat = "summary", fun.y="sum") + 
        labs(y = "Total PM2.5 Emissions (Tons)", x = "Year") +
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type") +
        geom_point

ggplot(bc, aes(factor(year), Emissions, fill = type)) +
        geom_bar(stat="identity") + guides(fill=FALSE) +
        facet_grid(.~type, scales = "fixed", space = "free") + 
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type") 


###########################################
## 4. Across the United States, how have emissions from coal
## combustion-related sources changed from 1999–2008?



## 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
## 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle emissions?
