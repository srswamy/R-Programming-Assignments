rankhospital <- function(state, outcome, num = "best") {

	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that state and outcome are valid
	states <- unique(data$State)
	outcomes <- c("heart attack", "heart failure", "pneumonia")

	if (state %in% states && outcome %in% outcomes) {
		index = -1;
		if (outcome == "heart attack") {
			index = 11
		} else if (outcome == "heart failure") {
			index = 17
		} else if (outcome == "pneumonia") {
			index = 23
		} else {
			print("Error")
		}
		
		subdata <- data[data$State == state,]
		subdata <- subset(subdata, subdata[,index] != "Not Available")

		## Sort the data based on the outcome and hospital names
		ordered <- subdata[order(as.numeric(subdata[,index]), subdata[,2]),]

		if (num == "best") {
			result <- ordered[1,2]
		} else if (num == "worst") {
			result <- ordered[nrow(ordered), 2]
		}
		else {
			if (as.numeric(num) > nrow(ordered)) {
				result <- c("NA")
			} else {
				result <- ordered[as.numeric(num), 2]
			}	
		}
		
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

}