Plot4 <- function() {
  # Packages
  library(dplyr)
  
  # Set Working Directory
  setwd("/Users/student/Documents/Learning/R/AirPollutionGraphs")
  
  # Read in Data from RDS files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Get rows in SCC for pollution from coal
  coal_SCC <- filter(SCC, grepl("Coal", EI.Sector))
  
  # Get rows in NEI for pollution from coal
  coal_NEI <- NEI[which(NEI$SCC %in% coal_SCC$SCC),]
  
  # Separating coal_NEI into data from 1999, 2002, 2005, 2008
  data_1999 <- subset(coal_NEI, coal_NEI$year == 1999)
  data_2002 <- subset(coal_NEI, coal_NEI$year == 2002)
  data_2005 <- subset(coal_NEI, coal_NEI$year == 2005)
  data_2008 <- subset(coal_NEI, coal_NEI$year == 2008)
  
  # Finding the total pollution due to coal for 1999, 2002, 2005, 2008 in kilotons
  total_pollution_1999 <- sum(data_1999$Emissions) / 1000
  total_pollution_2002 <- sum(data_2002$Emissions) / 1000
  total_pollution_2005 <- sum(data_2005$Emissions) / 1000
  total_pollution_2008 <- sum(data_2008$Emissions) / 1000
  
  # Create a vector of 1999, 2002, 2005, 2008
  year <- c(1999, 2002, 2005, 2008)
  
  # Create a vector of total pollution from 1999, 2002, 2005, 2008
  total_pollution <- c(total_pollution_1999, total_pollution_2002, total_pollution_2005, total_pollution_2008)
  
  # Create the PNG Device
  png(filename = "Plot4.png")
  
  # Create a plot of total pollution by year, then set the axes
  barplot(height = total_pollution, names.arg = year, xlab = "Year", ylab = "Total Pollution (Kilotons)", main = "Total Pollution vs. Year Due To Coal Combustion Sources", ylim = c(0, 600))
  #axis(2, c(0, 100, 200, 300, 400, 500, 600))
  
  # Turn the PNG device off
  dev.off()
}