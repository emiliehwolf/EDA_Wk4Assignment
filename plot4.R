########################################################################
## plot4.R
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
SCC <- readRDS("Source_Classification_Code.rds")

## Identify the codes pertaining to coal combustion
scc <- subset(SCC, EI.Sector=="Fuel Comb - Electric Generation - Coal" | 
                      EI.Sector=="Fuel Comb - Industrial Boilers, ICEs - Coal" | 
                      EI.Sector=="Fuel Comb - Comm/Institutional - Coal",
              select = SCC)

## Subset the rows that match the codes
CoalCombustion <- subset(NEI, SCC %in% scc$SCC)

## Get ready to plot
library(ggplot2)
theme_set(theme_bw(base_size = 14))

## A nice line graph
plot4 <- ggplot(CoalCombustion, aes(year, Emissions*0.000907185)) + 
        geom_line(stat = "summary", fun.y = "sum") + 
        labs(y = "PM2.5 Emissions in Gigagrams", x = "Year") +
        labs(title = "PM2.5 Coal Combustion Related Emissions")

## Save
ggsave(file = "plot4.png", plot4, width = 6.4, height = 4.8, units = "in")