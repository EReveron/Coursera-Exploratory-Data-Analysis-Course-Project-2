plot2 <- function() {   
	## plot2.R
	## Create Plot2 requested by the Coursera Exploratory Data Analysis Course Project 2
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


	## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
	## from 1999 to 2008? Use the base plotting system to make a plot answering this question.

	## Select the rows related with Maryland (fips==24510) and Aggregate (sum) the emissions by year
	dt <- NEI[which(NEI$fips=="24510"),]

	dt <- aggregate(Emissions ~ year, dt, sum)
	
	## Create a png file and Plot it

	png(filename="plot2.png", 
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
