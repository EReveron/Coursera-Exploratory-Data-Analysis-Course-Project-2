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


	## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
	## Using the base plotting system, make a plot showing the total PM2.5 emission from 
	## all sources for each of the years 1999, 2002, 2005, and 2008.

	dt <- aggregate(Emissions ~ year, NEI, sum)
	

	## Change emission scale to enhance the plot

	dt$Emissions <- dt$Emissions / 1000000
	
	## Create a png file, a Histogram and Plot it

	png(filename="plot1.png", 
    		units="px", 
    		width=480, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	barplot(height=dt$Emissions, names.arg=dt$year, xlab="years", 
		ylab="total PM2.5 emissions (millions of tons)",
		main="United States Total PM2.5 Emissions (1999-2008)", col = "blue")

	dev.off()

}