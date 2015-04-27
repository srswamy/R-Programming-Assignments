best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that state and outcome are valid
	states <- unique(data$State)
	outcomes <- c("heart attack", "heart failure", "pneumonia")

	if (state %in% states && outcome %in% outcomes) {
		index = -1;
		if (outcome == "heart attack") {
			index = 13
		} else if (outcome == "heart failure") {
			index = 19
		} else if (outcome == "pneumonia") {
			index = 25
		} else {
			print("Error")
		}
		
		subdata <- data[data$State == state,]

		## Sort the data based on the outcome and hospital names
		ordered <- subdata[order(as.numeric(subdata[,index]), subdata[,2]),]
		result <- ordered[1,2]
		return(result)
	}
	else if (!(state %in% states)) {
		stop("invalid state")
		geterrmessage()
	}
	else if (!(outcome %in% outcomes)) {
		stop("invalid outcome")
		geterrmessage()
	}
	else {
		print("Sorry, cannot perform calculation.")
	}


	## Return hospital name in that state with lowest 30-day death
	## rate
}