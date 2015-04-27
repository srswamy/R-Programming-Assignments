rankall <- function(outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name


	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	## Grab all the unique states:
	states <- unique(data$State)
	
	## Check that state and outcome are valid
	outcomes <- c("heart attack", "heart failure", "pneumonia")

	if (outcome %in% outcomes) {
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
			hospital <- character()
			state <- character()
			for (st in states) {
				subdata <- data[data$State == st,]
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
				hospital <- c(hospital, result)
				state <- c(state, st)
			}
			df <- data.frame(hospital, state)
			##df <- df[!(df[,1] == "NA"),]
			df <- df[order(df[,2]),]
			row.names(df) <- unique(df$state)
			return(df)
	}
	else {
		print("Sorry, cannot perform calculation.")
	}


}