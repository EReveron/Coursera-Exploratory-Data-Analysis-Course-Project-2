plot5 <- function() {   
	## plot5.R
	## Create Plot5 requested by the Coursera Exploratory Data Analysis Course Project 2
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


	## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 

	## Choose the SCC related with motor vehicle sources 

	coal_related <- grep("Vehicle",SCC$EI.Sector)

	coal_related <- SCC$SCC[coal_related]

	## Only choose the rows related with motor vehicle and Baltimore	

	dt <- NEI[which((NEI$SCC %in% coal_related) & NEI$fips=="24510"),]
	
	## Aggregate Emissions by year

	dt <- aggregate(Emissions ~ year, dt, sum)
		
	## Create a png file and Plot it

	png(filename="plot5.png", 
    		units="px", 
    		width=640, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	library(ggplot2)
	g <- qplot(factor(year), Emissions, data = dt, geom ="bar", stat = "identity",
		xlab="years", 
		ylab="total PM2.5 emissions (tons)",
		main="Baltimore City Motor Vehicle Emissions (1999-2008)")
  print(g)
	dev.off()

}
