# EDA_Wk4Assignment

Course Project 2 for Week 4 of Exploratory Data Analysis class on Coursera by Johns Hopkins Bloomberg School of Public Health.

Download the [zip](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip) and extract the two files.


Read datasets into R
```r
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
```

Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

```r
totals <- with(NEI, tapply(Emissions, year, sum, na.rm = T))
barplot(totals*.00000090718474, main = "Total PM2.5 Emissions From All Sources",
        ylab = "Teragrams of PM2.5", xlab = "Year", col = "tan")
```
![plot1](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plot1.png)

Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008? Use the base plotting system to make a plot answering this question.

```r
bc <- subset(NEI, fips == "24510")
totals <- with(bc, tapply(Emissions, year, sum, na.rm=T))
barplot(totals, main = "Total PM2.5 Emissions for Baltimore City, Maryland \nwith Linear Fit Model", 
        ylab = "PM2.5 Emissions (Tons)", xlab = "Year", col = "lightblue")
x <- 1:4
y <- as.vector(totals)
fit <- lm(y ~ x)
abline(fit, lty = "dashed")
```
![plot2](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plot2.png)

Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot to answer this question.
```r
library(ggplot2)
theme_set(theme_bw(base_size = 14))
bc <- subset(NEI, fips == "24510", c(Emissions,type,year))

ggplot(bc, aes(year, Emissions, color = type)) +
        geom_line(stat = "summary", fun.y="sum") + 
        labs(y = "Total PM2.5 Emissions (Tons)", x = "Year") +
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type")
```
![plota](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plota.png)
```r
ggplot(bc, aes(factor(year), Emissions, fill = type)) +
        geom_bar(stat="identity") + guides(fill=FALSE) +
        facet_grid(.~type, scales = "fixed", space = "free") + 
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD by Source Type") 
```
![plotb](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plotb.png)
```r
totals <- aggregate(Emissions ~ year + type, data = bc, FUN = "sum")
ggplot(totals, aes(year,Emissions,color=type)) + geom_line() + 
        geom_point(aes(shape = type), size = 3) +
        labs(x = "Year", y = "Total PM2.5 Emissions (Tons)") + 
        labs(title = "PM2.5 Emissions for Baltimore City, MD \nby Source Type")
```
![plot3](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plot3.png)

Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
```r
scc <- subset(SCC, EI.Sector=="Fuel Comb - Electric Generation - Coal" | 
                EI.Sector=="Fuel Comb - Industrial Boilers, ICEs - Coal" | 
                EI.Sector=="Fuel Comb - Comm/Institutional - Coal",
                select = SCC)
CoalCombustion <- subset(NEI, SCC %in% scc$SCC)

ggplot(CoalCombustion, aes(year, Emissions*0.000907185)) + 
        geom_line(stat = "summary", fun.y = "sum") + 
        labs(y = "PM2.5 Emissions in Gigagrams", x = "Year") +
        labs(title = "PM2.5 Coal Combustion Related Emissions")
```
![plot4](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plot4.png)
```r
totals <- with(CoalCombustion, tapply(Emissions, year, sum, na.rm = T))
barplot(totals*0.000907185, main = "PM2.5 Emissions From Coal Combustion Sources",
        ylab = "Gigagrams of PM2.5", xlab = "Year")
```
![plotc](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plotc.png)

How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
```r
bcmv <- subset(NEI, fips == "24510" & type == "ON-ROAD", select = c(Emissions,year))
ggplot(bcmv, aes(as.factor(year), Emissions)) + 
        geom_bar(stat = "summary", fun.y = "sum", fill = "#cc3300") + 
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "PM2.5 Motor Vehicle Emissions in Baltimore City, MD")
```
![plot5](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plot5.png)

Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle emissions?
```r
mv <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD", 
        select = c(Emissions,year,fips))
totals <- aggregate(Emissions ~ year + fips, data = mv, FUN = "sum")

ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "PM2.5 Emissions in Tons", x = "Year") +
        labs(title = "Comparison of Motor Vehicle Emissions") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))
```
![plotd](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plotd.png)
```r
totals[1:4,3] <- totals[1:4,3] - totals[1,3]
totals[5:8,3] <- totals[5:8,3] - totals[5,3]

ggplot() + geom_line(data = totals, aes(x = as.factor(year), 
        y = Emissions, group=fips, color = fips)) +
        labs(y = "Change in PM2.5 Emissions (tons)", x = "Year") +
        labs(title = "Comparison of Motor Vehicle Emissions") + 
        theme(legend.title=element_blank()) + 
        scale_color_hue(labels=c("Los Angeles\nCounty", "Baltimore City"))
```
![plote](https://github.com/emiliehwolf/EDA_Wk4Assignment/blob/master/plote.png)
