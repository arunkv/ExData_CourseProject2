# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

library(dplyr)

# Load data 
# Cleanup so that only city, years of interest, emissions are in the data
NEI <- readRDS("summarySCC_PM25.rds")
NEI <- tbl_df(NEI) %>%
    filter(fips == "24510" & year >= 1999 & year <= 2008) %>%
    select(Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions))
colnames(NEI) <- c("year", "total")

# Plot PM2.5 totals for Baltimore from 1999 to 2008
png("plot2.png", width = 480, height = 480)
model <- lm(total ~ as.numeric(factor(year)), NEI)
with(NEI, barplot(total, 
                  names.arg=year,
                  xlab="Years", 
                  ylab="Total PM2.5 Emissions (in tons)", 
                  ylim=c(0, ceiling(coef(model)[1])), # Extend y axis to intercept of linear model
                  main="PM2.5 Emissions Trend in Baltimore"))
abline(model, lwd=2, col="red")
dev.off()
