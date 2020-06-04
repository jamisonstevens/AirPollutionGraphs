Plot3 <- function() {
  # Packages
  library(ggplot2)
  library(ggpubr)
  
  # Set Working Directory
  setwd("/Users/student/Documents/Learning/R/AirPollutionGraphs")
  
  # Read in Data from RDS files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Subsetting NEI for Baltimore City by year and type
  data_1999_point <- subset(NEI, NEI$year == 1999 & NEI$fips == "24510" & type == "POINT")
  data_1999_nonpoint <- subset(NEI, NEI$year == 1999 & NEI$fips == "24510" & type == "NONPOINT")
  data_1999_onroad <- subset(NEI, NEI$year == 1999 & NEI$fips == "24510" & type == "ON-ROAD")
  data_1999_nonroad <- subset(NEI, NEI$year == 1999 & NEI$fips == "24510" & type == "NON-ROAD")
  data_2002_point <- subset(NEI, NEI$year == 2002 & NEI$fips == "24510" & type == "POINT")
  data_2002_nonpoint <- subset(NEI, NEI$year == 2002 & NEI$fips == "24510" & type == "NONPOINT")
  data_2002_onroad <- subset(NEI, NEI$year == 2002 & NEI$fips == "24510" & type == "ON-ROAD")
  data_2002_nonroad <- subset(NEI, NEI$year == 2002 & NEI$fips == "24510" & type == "NON-ROAD")
  data_2005_point <- subset(NEI, NEI$year == 2005 & NEI$fips == "24510" & type == "POINT")
  data_2005_nonpoint <- subset(NEI, NEI$year == 2005 & NEI$fips == "24510" & type == "NONPOINT")
  data_2005_onroad <- subset(NEI, NEI$year == 2005 & NEI$fips == "24510" & type == "ON-ROAD")
  data_2005_nonroad <- subset(NEI, NEI$year == 2005 & NEI$fips == "24510" & type == "NON-ROAD")
  data_2008_point <- subset(NEI, NEI$year == 2008 & NEI$fips == "24510" & type == "POINT")
  data_2008_nonpoint <- subset(NEI, NEI$year == 2008 & NEI$fips == "24510" & type == "NONPOINT")
  data_2008_onroad <- subset(NEI, NEI$year == 2008 & NEI$fips == "24510" & type == "ON-ROAD")
  data_2008_nonroad <- subset(NEI, NEI$year == 2008 & NEI$fips == "24510" & type == "NON-ROAD")
  
  # Calculating total emissions for Baltimore City by year and type
  total_point_1999 <- sum(data_1999_point$Emissions)
  total_nonpoint_1999 <- sum(data_1999_nonpoint$Emissions)
  total_onroad_1999 <- sum(data_1999_onroad$Emissions)
  total_nonroad_1999 <- sum(data_1999_nonroad$Emissions)
  total_point_2002 <- sum(data_2002_point$Emissions)
  total_nonpoint_2002 <- sum(data_2002_nonpoint$Emissions)
  total_onroad_2002 <- sum(data_2002_onroad$Emissions)
  total_nonroad_2002 <- sum(data_2002_nonroad$Emissions)
  total_point_2005 <- sum(data_2005_point$Emissions)
  total_nonpoint_2005 <- sum(data_2005_nonpoint$Emissions)
  total_onroad_2005 <- sum(data_2005_onroad$Emissions)
  total_nonroad_2005 <- sum(data_2005_nonroad$Emissions)
  total_point_2008 <- sum(data_2008_point$Emissions)
  total_nonpoint_2008 <- sum(data_2008_nonpoint$Emissions)
  total_onroad_2008 <- sum(data_2008_onroad$Emissions)
  total_nonroad_2008 <- sum(data_2008_nonroad$Emissions)
  
  # Creating vector of years
  yearVector <- c(1999, 2002, 2005, 2008)
  
  # Creating vectors for each type of pollution for the years 1999, 2002, 2005, 2008
  point_pollution <- c(total_point_1999, total_point_2002, total_point_2005, total_point_2008)
  nonpoint_pollution <- c(total_nonpoint_1999, total_nonpoint_2002, total_nonpoint_2005, total_nonpoint_2008)
  onroad_pollution <- c(total_onroad_1999, total_onroad_2002, total_onroad_2005, total_onroad_2008)
  nonroad_pollution <- c(total_nonroad_1999, total_nonroad_2002, total_nonroad_2005, total_nonroad_2008)
  
  # Creating a data frame from the vectors for years, 
  type_pollution <- data.frame(yearVector, point_pollution, nonpoint_pollution, onroad_pollution, nonroad_pollution)
  
  # Creating PNG Device
  png(filename = "Plot3.png")
  
  # Creating, formatting, and printing plot
  g_point <- ggplot(type_pollution, aes(x = as.character(yearVector), y = point_pollution)) + geom_bar(stat = "identity", color = "red")
  g_point <- g_point + ggtitle("Baltimore City Point Pollution") + xlab("Year") + ylab("Total Point Pollution (Tons)") + theme(plot.title = element_text(hjust = 0.5))
  g_nonpoint <- ggplot(type_pollution, aes(x = as.character(yearVector), y = nonpoint_pollution)) + geom_bar(stat = "identity", color = "blue")
  g_nonpoint <- g_nonpoint + ggtitle("Baltimore City Nonpoint Pollution") + xlab("Year") + ylab("Total Nonpoint Pollution (Tons)") + theme(plot.title = element_text(hjust = 0.5))
  g_onroad <- ggplot(type_pollution, aes(x = as.character(yearVector), y = onroad_pollution)) + geom_bar(stat = "identity", color = "green")
  g_onroad <- g_onroad + ggtitle("Baltimore City Onroad Pollution") + xlab("Year") + ylab("Total Onroad Pollution (Tons)") + theme(plot.title = element_text(hjust = 0.5))
  g_nonroad <- ggplot(type_pollution, aes(x = as.character(yearVector), y = nonroad_pollution)) + geom_bar(stat = "identity", color = "purple")
  g_nonroad <- g_nonroad + ggtitle("Baltimore City Nonroad Pollution") + xlab("Year") + ylab("Total Nonroad Pollution (Tons)") + theme(plot.title = element_text(hjust = 0.5))
  print(ggarrange(g_point, g_nonpoint, g_onroad, g_nonroad, nrow = 2, ncol = 2))
  
  # Turning PNG Device off
  dev.off()
}