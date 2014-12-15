# Copyright 2014 Arun K Viswanathan
# All rights reserved

# Exploratory Data Analysis
# Course Project 2

# Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in 
# emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.

library(dplyr)
library(ggplot2)

# Load data 
# To avoid repeated loads, check if data was already loaded in current session
# Cleanup so that only city, years of interest, emissions are in the data
NEI <- readRDS("summarySCC_PM25.rds")
NEI <- tbl_df(NEI) %>%
    filter(fips == "24510" & year >= 1999 & year <= 2008) %>%
    select(type, Emissions, year) %>%        
    mutate(year = as.factor(year)) %>%
    mutate(type = as.factor(type)) %>% 
    group_by(year, type) %>%
    summarize(sum(Emissions))
colnames(NEI) <- c("year", "type", "total")

# Plot PM2.5 totals for Baltimore from 1999 to 2008 by source type
png("plot3.png", width = 480, height = 960)

plot <- ggplot(NEI, aes(x=year, y=total, fill=type)) + # Total by year
    scale_fill_discrete(name="Source Type") +          # Fill by type
    facet_grid(type~.) +                               # Separate facets by type
    ggtitle("PM2.5 Totals in Baltimore") +             # Main title
    xlab("Years") +                                    # X axis label
    ylab("Total PM2.5 Emissions (in tons)") +          # Y axis label
    stat_boxplot(geom="bar", linetype=1) +             # Plot bars 
    stat_smooth(method="lm", aes(group=type), se=F)    # Linear model line

print(plot)
dev.off()
