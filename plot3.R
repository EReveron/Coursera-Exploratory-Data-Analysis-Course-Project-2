plot3 <- function() {   
	## plot3.R
	


	## Written by: Enrique Reveron

	## Set system locale to English to have the same output requested (strptime)
	Sys.setlocale("LC_TIME", "English")

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


	## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
	## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
	## Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
	## a plot answer this question.


	dt <- NEI[which(NEI$fips=="24510"),]

	dt <- aggregate(Emissions ~ year + type , dt, sum)
	


	
	## Create a png file and Plot it

	png(filename="plot3.png", 
    		units="px", 
    		width=480, 
    		height=480, 
    		pointsize=12, 
    		res=72)

	barplot(height=dt$Emissions, names.arg=dt$year, xlab="years", 
		ylab=expression('total PM'[2.5]*' emission'),main=expression('Total PM'[2.5]*' emissions at Baltimore in various years'))

	dev.off()

}
