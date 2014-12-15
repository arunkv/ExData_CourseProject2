# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

library(dplyr)
library(ggplot2)

# Load data 

# Load the classfication codes 
SCC <- readRDS("Source_Classification_Code.rds")

# Get all the classification codes that are related to motor vehicles
# Assumption: If the name contains "Vehicle", it is related to motor vehicles
SCC <- tbl_df(SCC) %>%
    filter(grepl("Vehicle", Short.Name, ignore.case=TRUE)) %>%
    select(SCC)

# Load the PM2.5 data
NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))

# Filter out the motor vehicle related data in Baltimore from 1999 to 2008
NEI_Baltimore <- NEI %>%
    filter(fips == "24510" & year >= 1999 & year <= 2008 & SCC %in% SCC$SCC) %>%
    select(Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions)) %>%
    mutate(region = "Baltimore City")
colnames(NEI_Baltimore) <- c("year", "total", "region")

# Filter out the motor vehicle related data in BLA from 1999 to 2008
NEI_LA <- NEI %>%
    filter(fips == "06037" & year >= 1999 & year <= 2008 & SCC %in% SCC$SCC) %>%
    select(Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions)) %>%
    mutate(region = "Los Angeles County")
colnames(NEI_LA) <- c("year", "total", "region")

# Combine the Baltimore and LA data into one table
NEI <- rbind(NEI_Baltimore, NEI_LA) %>%
    mutate(region = as.factor(region)) %>% 
    group_by(year, region)

# Plot PM2.5 emissions from motor vehicle related sources in Baltimore
png("plot6.png", width = 480, height = 480)
plot <- ggplot(NEI, aes(x=year, y=total, fill=region)) + # Total by year
    scale_fill_discrete(name="Region") +                 # Fill by type
    ggtitle("PM2.5 Totals: Motor Vehicles: Baltimore vs. Los Angeles") + # Main title
    xlab("Years") +                                      # X axis label
    ylab("Total PM2.5 Emissions (in tons)") +            # Y axis label
    stat_boxplot(geom="bar", linetype=1) +               # Plot bars 
    stat_smooth(method="lm", aes(group=region), se=F)    # Linear model line
print(plot)
dev.off()
