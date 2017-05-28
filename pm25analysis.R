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

## Plot lines without manipulating data
ggplot(bc, aes(year, Emissions, color = type)) +
        geom_line(stat = "summary", fun.y="sum") + 
        labs(y = "Total PM2.5 Emissions (Tons)", x = "Year") +
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type")

## 4-panel boxplot
ggplot(bc, aes(factor(year), Emissions, fill = type)) +
        geom_bar(stat="identity") + guides(fill=FALSE) +
        facet_grid(.~type, scales = "fixed", space = "free") + 
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type") 

## totals <- xtabs(Emissions ~ year + type, bc)

## Aggregate data and plot lines
totals <- aggregate(Emissions ~ year + type, data = bc, FUN = "sum")
ggplot(totals, aes(year,Emissions,color=type)) + geom_line() + 
        geom_point(aes(shape = type), size = 3) +
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD \nby Source Type")


###########################################
## 4. Across the United States, how have emissions from coal
## combustion-related sources changed from 1999–2008?

##ccindices <- intersect(union(grep("comb ", SCC$Short.Name, ignore.case = T, value = T), grep("combustion", SCC$Short.Name, ignore.case = T, value = T)),grep("coal ", SCC$Short.Name, ignore.case = T, value = T))
##CoalCombustionSCC <- subset(SCC, EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal","Fuel Comb - Electric Generation - Coal","Fuel Comb - Industrial Boilers, ICEs - Coal"))

scc <- subset(SCC, EI.Sector=="Fuel Comb - Electric Generation - Coal" | 
                EI.Sector=="Fuel Comb - Industrial Boilers, ICEs - Coal" | 
                EI.Sector=="Fuel Comb - Comm/Institutional - Coal",
                select = SCC)
CoalCombustion <- subset(NEI, SCC %in% scc$SCC)

ggplot(CoalCombustion, aes(year, Emissions*0.000907185)) + 
        geom_line(stat = "summary", fun.y = "sum") + 
        labs(y = "PM2.5 Emissions in Gigagrams", x = "Year") +
        labs(title = "PM2.5 Coal Combustion Related Emissions")

totals <- with(CoalCombustion, tapply(Emissions, year, sum, na.rm = T))
barplot(totals*0.000907185, main = "PM2.5 Emissions From Coal Combustion Sources",
        ylab = "Gigagrams of PM2.5", xlab = "Year")

###############################################
## 5. How have emissions from motor vehicle sources changed 
## from 1999–2008 in Baltimore City?

bcmv <- subset(NEI, fips == "24510" & type == "ON-ROAD", select = c(Emissions,year))
ggplot(bcmv, aes(as.factor(year), Emissions)) + 
        geom_bar(stat = "summary", fun.y = "sum", fill = "#cc3300") + 
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "PM2.5 Motor Vehicle Emissions in Baltimore City, MD")

###############################################
## 6. Compare emissions from motor vehicle sources in Baltimore City
## with emissions from motor vehicle sources in Los Angeles County,
## California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater
## changes over time in motor vehicle emissions?

mv <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD", 
        select = c(Emissions,year,fips))
totals <- aggregate(Emissions ~ year + fips, data = mv, FUN = "sum")

plot1 <- ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "Comparison of Motor Vehicle Emissions") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))

totals[1:4,3] <- totals[1:4,3] - totals[1,3]
totals[5:8,3] <- totals[5:8,3] - totals[5,3]

plot2 <- ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "Change in PM2.5 Emissions (tons)", x = "Year") +
        labs(title = "Comparison of Motor Vehicle Emissions") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))

library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)
