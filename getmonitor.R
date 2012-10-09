
getmonitor<-function(id,directory,summarize = FALSE) {


	if(id<10) {
		fp<-paste(directory,"/00",as.character(id),".csv",sep="")
	}
	else if(id<100) {
		fp<-paste(directory,"/0",as.character(id),".csv",sep="")
	}
	else {
		fp<-paste(directory,"/",as.character(id),".csv",sep="")
	}
	
	monitor_data <- read.csv(fp, header=T)
	
	if(summarize==TRUE) {
		print(summary(monitor_data))
	}
	
	return(invisible(monitor_data))
}

