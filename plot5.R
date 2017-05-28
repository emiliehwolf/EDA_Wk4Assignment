########################################################################
## plot5.R
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

## Subset the rows both in Baltimore City and from on-road sources
bcmv <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD",]

## Get ready to plot
library(ggplot2)
theme_set(theme_bw(base_size = 14))

## A nice bar graph
plot5 <- ggplot(bcmv, aes(as.factor(year), Emissions)) + 
        geom_bar(stat = "summary", fun.y = "sum", fill = "#cc3300") + 
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "PM2.5 Motor Vehicle Emissions in Baltimore City, MD")

## Save
ggsave(file = "plot5.png", plot5, width = 6.4, height = 4.8, units = "in")