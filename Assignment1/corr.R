corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	## Get the complete cases for all of the monitors by calling Complete
	source('complete.R')
	ignore <- capture.output(data <- complete(directory))
	
	data <- subset(data, nobs >= threshold)
	output <- c()
	
	for (i in data[,1]) {

		path <- file.path(paste(getwd(), directory, sep = "/"), paste(formatC(i, width = 3, format = "d", flag = "0"), ".csv", sep = ""))
		
		result <- read.csv(path, TRUE)
		
		completeCases <- result[complete.cases(result), ]		

		output <- append(output, cor(completeCases$sulfate, completeCases$nitrate))

	}
	return (output)
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
}