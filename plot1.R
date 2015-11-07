plot1 <- function() {   
	## plot1.R
	## Create Plot1 requested by the Coursera Exploratory Data Analysis Course Project 2
	## Written by: Enrique Reveron


	## Check if the data file is located in the working dir

	if (!file.exists("summarySCC_PM25.rds"))
	{
		{ stop("no valid data file in working directory:summarySCC_PM25.rds") }
	}

	if (!file.exists("Source_Classification_Code.rds"))
	{
		{ stop("no valid data file in working directory: Source_Classification_Code.rds") }
	}

	## Read the Information

	NEI <- readRDS("summarySCC_PM25.rds")
	SCC <- readRDS("Source_Classification_Code.rds")


	## Question: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
	## Using the base plotting system, make a plot showing the total PM2.5 emission from 
	## all sources for each of the years 1999, 2002, 2005, and 2008.

	## Aggregate the Data of all the Emissions by Year
	dt <- aggregate(Emissions ~ year, NEI, sum)
	
	## Create a png file and Plot it

	png(filename="plot1.png", 
    		units="px", 
    		width=480, 
    		height=480,  
    		pointsize=12,  
    		res=72)

	## Change emission scale to enhance the plot

	dt_emissions_scale <- dt$Emissions / 1000000
	
	barplot(height=dt_emissions_scale, names.arg=dt$year, xlab="years", 
		ylab="total PM2.5 emissions (millions of tons)",
		main="United States Total PM2.5 Emissions (1999-2008)", col = "blue")

	dev.off()

}
