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
	
	## Chooose the rows related with Baltimore City (fips == 24510) and
	## aggregate the emission data by year and type

	dt <- NEI[which(NEI$fips=="24510"),]

	dt <- aggregate(Emissions ~ year + type , dt, sum)
		
	## Create a png file, Plot the graph using ggplot2


	png(filename="plot3.png", 
    		units="px", 
    		width=480, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	library(ggplot2)
	
	g <- qplot(year, Emissions, data = dt, fill = type, color = type, geom ="line", 
		xlab = "year", ylab = "Total PM2.5 Emissions (tons)",
		main = "Total Emissions in Baltimore City, Maryland from 1999 to 2008")
	print(g)
	dev.off()
}