Plot6 <- function() {
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
  balt_city_data_1999 <- subset(vehicle_NEI, vehicle_NEI$year == 1999 & vehicle_NEI$fips == "24510")
  balt_city_data_2002 <- subset(vehicle_NEI, vehicle_NEI$year == 2002 & vehicle_NEI$fips == "24510")
  balt_city_data_2005 <- subset(vehicle_NEI, vehicle_NEI$year == 2005 & vehicle_NEI$fips == "24510")
  balt_city_data_2008 <- subset(vehicle_NEI, vehicle_NEI$year == 2008 & vehicle_NEI$fips == "24510")
  la_cty_data_1999 <- subset(vehicle_NEI, vehicle_NEI$year == 1999 & vehicle_NEI$fips == "06037")
  la_cty_data_2002 <- subset(vehicle_NEI, vehicle_NEI$year == 2002 & vehicle_NEI$fips == "06037")
  la_cty_data_2005 <- subset(vehicle_NEI, vehicle_NEI$year == 2005 & vehicle_NEI$fips == "06037")
  la_cty_data_2008 <- subset(vehicle_NEI, vehicle_NEI$year == 2008 & vehicle_NEI$fips == "06037")
  
  # Finding the total pollution due to vehicles for 1999, 2002, 2005, 2008 in tons
  balt_city_pollution_1999 <- sum(balt_city_data_1999$Emissions)
  balt_city_pollution_2002 <- sum(balt_city_data_2002$Emissions)
  balt_city_pollution_2005 <- sum(balt_city_data_2005$Emissions)
  balt_city_pollution_2008 <- sum(balt_city_data_2008$Emissions)
  la_cty_pollution_1999 <- sum(la_cty_data_1999$Emissions)
  la_cty_pollution_2002 <- sum(la_cty_data_2002$Emissions)
  la_cty_pollution_2005 <- sum(la_cty_data_2005$Emissions)
  la_cty_pollution_2008 <- sum(la_cty_data_2008$Emissions)
  
  # Create a vector of 1999, 2002, 2005, 2008
  year <- c(1999, 2002, 2005, 2008)
  
  # Create a vector of total pollution from 1999, 2002, 2005, 2008
  total_balt_city_pollution <- c(balt_city_pollution_1999, balt_city_pollution_2002, balt_city_pollution_2005, balt_city_pollution_2008)
  total_la_cty_pollution <- c(la_cty_pollution_1999, la_cty_pollution_2002, la_cty_pollution_2005, la_cty_pollution_2008)
  
  # Create the PNG Device
  png(filename = "Plot6.png")
  
  # Create a plot of total pollution by year, then set the axes
  par(mfrow = c(1, 2))
  barplot(height = total_balt_city_pollution, names.arg = year, xlab = "Year", ylab = "Total Pollution (Tons)", main = "Baltimore City", ylim = c(0,400))
  axis(2, c(0, 50, 100, 150, 200, 250, 300, 350, 400))
  barplot(height = total_la_cty_pollution, names.arg = year, xlab = "Year", ylab = "Total Pollution (Tons)", main = "Los Angeles County", ylim = c(0, 5000))
  axis(2, c(0, 1000, 2000, 3000, 4000, 5000))
  
  # Turn the PNG device off
  dev.off()
}