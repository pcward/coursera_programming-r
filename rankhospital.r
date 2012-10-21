rankhospital <- function(state, outcome, num = "best") {

	# Read outcome data
	options(warn=-1)
	state<-as.character(state)
	outcome<-as.character(outcome)
	num<-as.character(num)

	d<-read.csv("outcome-of-care-measures.csv", header=T, colClasses="character")

	# Check that state is valid
	state_ok<-match(state,d$State,nomatch=0)

	if(state_ok!=0) {

		cand<-as.character(d$Hospital.Name[state==d$State])

		if(outcome == "pneumonia") {	# Check that condition is valid
			scores<-as.numeric(as.character(d$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[state==d$State]))

			cand_df<-data.frame(cand,scores)
			
			cands<-cand_df[with(cand_df, order(scores,cand)), ]
			cands$cand<-as.character(cands[,1])

			max<-nrow(cands)+1
			rownames(cands)<-c(2:max)

		if(num=="best") {
			rank<-1
		}
		else if(num=="worst") {
			rank<-max
		}
		else {
			rank<-as.integer(num)

			if(rank>(max-1)) {
				#print("out of bounds NA")
			}
		}

			# Return hospital name in that state with lowest 30-day death rate for the given outcome
			print(cands$cand[rank])

		}
		else if (outcome == "heart attack") {	# Check that condition is valid

			scores<-as.numeric(as.character(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[state==d$State]))

			cand_df<-data.frame(cand,scores)
			
			cands<-cand_df[with(cand_df, order(scores,cand)), ]
			cands$cand<-as.character(cands[,1])

			max<-nrow(cands)+1
			rownames(cands)<-c(2:max)

		if(num=="best") {
			rank<-1
		}
		else if(num=="worst") {
			rank<-max
		}
		else {
			rank<-as.integer(num)

			if(rank>(max-1)) {
				#print("out of bounds NA")
			}
		}

			# Return hospital name in that state with lowest 30-day death rate for the given outcome
			print(cands$cand[rank])
		}
		else if (outcome == "heart failure") {	# Check that condition is valid
			scores<-as.numeric(as.character(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[state==d$State]))

			cand_df<-data.frame(cand,scores)
			
			cands<-cand_df[with(cand_df, order(scores,cand)), ]
			cands$cand<-as.character(cands[,1])

			max<-nrow(cands)+1
			rownames(cands)<-c(2:max)

		if(num=="best") {
			rank<-1
		}
		else if(num=="worst") {
			rank<-max
		}
		else {
			rank<-as.integer(num)

			if(rank>(max-1)) {
				#print("out of bounds NA")
			}
		}

			# Return hospital name in that state with lowest 30-day death rate for the given outcome
			print(cands$cand[rank])

		}
		else {
			stop("invalid outcome")	# Invalid outcome
		}
	}
	else {
		stop("invalid state")	# Invalid state
	}

}
