Plot5 <- function() {
  # Packages
  library(dplyr)
  
  # Set Working Directory
  setwd("/Users/student/Documents/Learning/R/AirPollutionGraphs")
  
  # Read in Data from RDS files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Get rows in SCC for pollution from Vehicles
  vehicle_SCC <- filter(SCC, grepl("Vehicle", EI.Sector))
  
  # Get rows in NEI for pollution from vehicles
  vehicle_NEI <- NEI[which(NEI$SCC %in% vehicle_SCC$SCC),]
  
  # Separating vehicle_NEI into data from 1999, 2002, 2005, 2008
  data_1999 <- subset(vehicle_NEI, vehicle_NEI$year == 1999 & vehicle_NEI$fips == "24510")
  data_2002 <- subset(vehicle_NEI, vehicle_NEI$year == 2002 & vehicle_NEI$fips == "24510")
  data_2005 <- subset(vehicle_NEI, vehicle_NEI$year == 2005 & vehicle_NEI$fips == "24510")
  data_2008 <- subset(vehicle_NEI, vehicle_NEI$year == 2008 & vehicle_NEI$fips == "24510")
  
  # Finding the total pollution due to vehicles for 1999, 2002, 2005, 2008 in tons
  total_pollution_1999 <- sum(data_1999$Emissions)
  total_pollution_2002 <- sum(data_2002$Emissions)
  total_pollution_2005 <- sum(data_2005$Emissions)
  total_pollution_2008 <- sum(data_2008$Emissions)
  
  # Create a vector of 1999, 2002, 2005, 2008
  year <- c(1999, 2002, 2005, 2008)
  
  # Create a vector of total pollution from 1999, 2002, 2005, 2008
  total_pollution <- c(total_pollution_1999, total_pollution_2002, total_pollution_2005, total_pollution_2008)
  
  # Create the PNG Device
  png(filename = "Plot5.png")
  
  # Create a plot of total pollution by year, then set the axes
  barplot(height = total_pollution, names.arg = year, xlab = "Year", ylab = "Total Pollution (Tons)", main = "Total Pollution vs. Year Due To Vehicles in Baltimore City", ylim = c(0,400))
  axis(2, c(0, 50, 100, 150, 200, 250, 300, 350, 400))
  
  # Turn the PNG device off
  dev.off()
}