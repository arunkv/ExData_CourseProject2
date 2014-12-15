# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 
# 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# Upload a PNG file containing your plot addressing this question.

library(dplyr)

# Load data 
# Cleanup so that only years of interest and emissions are in the data
NEI <- readRDS("summarySCC_PM25.rds")
NEI <- tbl_df(NEI) %>%
    select(Emissions, year) %>%
    filter(year == 1999 | year == 2002 | year == 2005 | year == 2008) %>%
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions)/1000000) 
colnames(NEI) <- c("year", "total")

# Plot PM2.5 totals for 1999, 2002, 2005, 2008
png("plot1.png", width = 480, height = 480)
model <- lm(total ~ as.numeric(factor(year)), NEI)
with(NEI, barplot(total, 
                  names.arg=year,
                  xlab="Years", 
                  ylab="Total PM2.5 Emissions (in millions of tons)", 
                  ylim=c(0, ceiling(coef(model)[1])), # Extend y axis to intercept of linear model
                  main="PM2.5 Emissions Trend"))
abline(model, lwd=2, col="red")
dev.off()
