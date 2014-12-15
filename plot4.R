# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 4
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

library(dplyr)

# Load data 

# Load the classfication codes 
SCC <- readRDS("Source_Classification_Code.rds")

# Get all the classification codes that are related to coal
# Assumption: If the word "Coal" is in the name, it is related to coal
SCC <- tbl_df(SCC) %>%
    filter(grepl("Coal", Short.Name, ignore.case=TRUE)) %>%
    select(SCC)

# Load the PM2.5 data
NEI <- readRDS("summarySCC_PM25.rds")

# Filter out the coal related data for 1999 to 2008
NEI <- tbl_df(NEI) %>%
    filter(SCC %in% SCC$SCC) %>%
    select(Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarize(sum(Emissions)/1000)
colnames(NEI) <- c("year", "total")

# Plot PM2.5 emissions from coal-related sources 
png("plot4.png", width = 480, height = 480)
model <- lm(total ~ as.numeric(factor(year)), NEI)
with(NEI, barplot(total, 
                  names.arg=year,
                  xlab="Years", 
                  ylab="Total PM2.5 Emissions (in thousands of tons)", 
                  ylim=c(0, ceiling(coef(model)[1])), # Extend y axis to intercept of linear model
                  main="PM2.5 Emissions Related to Coal"))
abline(model, lwd=2, col="red")
dev.off()
