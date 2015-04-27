complete <- function(directory, id = 1:332) {

	paths <- file.path(paste(getwd(), directory, sep = "/"), paste(formatC(id, width = 3, format = "d", flag = "0"), ".csv", sep = ""))
	
	for (path in paths) {
		if (!exists("dataset") & !exists("monitors") & !exists("count")) {
			dataset <- read.csv(path, header = TRUE)
			dataset <- dataset[complete.cases(dataset),]
			monitors <- as.numeric(dataset[1,4])
			count <- as.numeric(nrow(dataset))
			
		}
		else if (exists("monitors") & exists("count")) {
			tmp <- read.csv(path)
			tdata <- tmp[complete.cases(tmp),]
			monitors <- append(monitors, as.numeric(tmp[1,4]))
			count <- append(count, as.numeric(nrow(tdata)))
			rm(tmp)
		}
	}
	
	result <- data.frame(monitors, count)
	names(result) <- c('id', 'nobs')
	
	
	print(result)
}