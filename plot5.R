# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 5
# How have emissions from motor vehicle sources changed from 1999–2008 in 
# Baltimore City?

library(dplyr)

# Load data 

# Load the classfication codes 
SCC <- readRDS("Source_Classification_Code.rds")

# Get all the classification codes that are related to motor vehicles
# Assumption: If the name contains "Vehicle", it is related to motor vehicles
SCC <- tbl_df(SCC) %>%
    filter(grepl("Vehicle", Short.Name, ignore.case=TRUE)) %>%
    select(SCC)

# Load the PM2.5 data
NEI <- readRDS("summarySCC_PM25.rds")

# Filter out the motor vehicle related data in Baltimore from 1999 to 2008
NEI <- tbl_df(NEI) %>%
    filter(fips == "24510" & year >= 1999 & year <= 2008 & SCC %in% SCC$SCC) %>%
    select(Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions))
colnames(NEI) <- c("year", "total")

# Plot PM2.5 emissions from motor vehicle related sources in Baltimore
png("plot5.png", width = 480, height = 480)
model <- lm(total ~ as.numeric(factor(year)), NEI)
with(NEI, barplot(total, 
                  names.arg=year,
                  xlab="Years", 
                  ylab="Total PM2.5 Emissions (in tons)", 
                  ylim=c(0, ceiling(coef(model)[1])), # Extend y axis to intercept of linear model
                  main="PM2.5 Emissions in Baltimore Related to Motor Vehicles"))
abline(model, lwd=2, col="red")
dev.off()
