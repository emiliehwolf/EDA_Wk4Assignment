# EDA_Wk4Assignment

Course Project 2 for Week 4 of Exploratory Data Analysis class on Coursera by Johns Hopkins Bloomberg School of Public Health.

#### URL of Data: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

Read in datasets
```r
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
```

1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
