plot4 <- function() {   
	## plot4.R
	## Create Plot4 requested by the Coursera Exploratory Data Analysis Course Project 2
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


	## Question: Across the United States, how have emissions from coal combustion-related sources changed 
	## from 1999–2008?

	## Choose the SCC related with Coal 

	coal_related <- grep("- Coal",SCC$EI.Sector)

	coal_related <- SCC$SCC[coal_related]

	## Only choose the rows related with Coal-Combustion and Aggregate the Emission by Year	

	dt <- NEI[which(NEI$SCC %in% coal_related),]

	dt <- aggregate(Emissions ~ year, dt, sum)
		
	## Create a png file, a Histogram and Plot it

	png(filename="plot4.png", 
    		units="px", 
    		width=640, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	## Change emission from millions to thousands to enhance the plot

	dt$Emissions <- dt$Emissions / 1000000

	library(ggplot2)
	qplot(year, Emissions, data = dt, geom ="bar", stat = "identity",
		xlab="years", 
		ylab="total PM2.5 emissions (millions of tons)",
		main="United States Total Coal Combustion-Related PM2.5 Emissions (1999-2008)")

	dev.off()

}
