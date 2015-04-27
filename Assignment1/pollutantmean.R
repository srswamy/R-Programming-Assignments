pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

	paths <- file.path(paste(getwd(), directory, sep = "/"), paste(formatC(id, width = 3, format = "d", flag = "0"), ".csv", sep = ""))
	
	for (path in paths) {
		if (!exists("dataset")) {
			dataset <- read.csv(path, header = TRUE)
		}
		
		if (exists("dataset")) {
			tmp <- read.csv(path)
			dataset <- rbind(dataset, tmp)
			rm(tmp)
		}
	
	}	
	
	if (pollutant == "sulfate") {
		ext_sulfates <- dataset["sulfate"]
		print(round(mean(ext_sulfates[!is.na(ext_sulfates)]),3))
		 
	} else if (pollutant == "nitrate") {
		ext_nitrates <- dataset["nitrate"]
		print(round(mean(ext_nitrates[!is.na(ext_nitrates)]),3))
	}

}