plot3 <- function() {   
	## plot3.R
	## Create plot3 requested by the Coursera Exploratory Data Analysis Course Project 2
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

	## Question: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
	## variable, which of these four sources have seen decreases in emissions from 1999–2008 for 
	## Baltimore City? Which have seen increases in emissions from 1999–2008? 
	## Use the ggplot2 plotting system to make a plot answer this question.

	dt <- NEI[which(NEI$fips=="24510"),]

	dt <- aggregate(Emissions ~ year + type , dt, sum)
		
	## Create a png file, a Histogram and Plot it

	png(filename="plot3.png", 
    		units="px", 
    		width=480, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	barplot(height=dt$Emissions, names.arg=dt$year, xlab="years", 
		ylab="total PM2.5 emissions (tons)",
		main="Total PM2.5 Emissions (1999-2008) at Baltimore City, Maryland", col = "blue")


	dev.off()

}
