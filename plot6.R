plot6 <- function() {   
	## plot6.R
	## Create Plot6 requested by the Coursera Exploratory Data Analysis Course Project 2
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

  ## Question: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
  ## vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater 
  ## changes over time in motor vehicle emissions?

	## Choose the SCC related with motor vehicle sources 

	coal_related <- grep("Vehicle",SCC$EI.Sector)

	coal_related <- SCC$SCC[coal_related]

	## Only choose the rows related with Coal-Combustion and Baltimore and Aggegate Emissions by Date	

	dt_baltimore <- NEI[which((NEI$SCC %in% coal_related) & NEI$fips=="24510"),]

	dt_baltimore <- aggregate(Emissions ~ year, dt_baltimore, sum)
	
	dt_baltimore[State,] <- "BALTIMORE"

	## Only choose the rows related with Coal-Combustion and Los Angeles and Aggregate Emissions by Date

	dt_la <- NEI[which((NEI$SCC %in% coal_related) & NEI$fips=="06037"),]

	dt_la <- aggregate(Emissions ~ year, dt_la, sum)
	
	dt_la[State,] <- "LOS ANGELES"
	
	dt2 <- rbind(dt_baltimore,dt_la)

	## Create a png file and Plot it

	png(filename="plot2.png", 
    		units="px", 
    		width=480, 
    		height=480, 
    		pointsize=12, 
    		res=72)


	library(ggplot2)
	
	qplot(year, Emissions, data = dt2, fill = State, color = State, geom ="line", 
		xlab = "year", ylab = "Total PM2.5 Emissions (tons)",
		main = "Baltimore and Los Angeles Total Vehicle Emissions from 1999 to 2008")
		
	dev.off()

}
