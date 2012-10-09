
complete <- function(directory, id = 1:332) {
       

        ids<-vector()
        numcomplete<-vector()
            
        doshit<-function(i) {
			if(i<10) {
				fp<-paste("specdata/00",as.character(i),".csv",sep="")
			}
			else if(i<100) {
				fp<-paste("specdata/0",as.character(i),".csv",sep="")
			}
			else {
				fp<-paste("specdata/",as.character(i),".csv",sep="")
			}  
			
			data <- read.csv(fp, header=T)
			data<-data[complete.cases(data),]
			
			return(c(i,nrow(data)))

        }

		results<-sapply(id,doshit)
					
        df<-as.data.frame(t(results))
        names(df)[1] <- "id"
        names(df)[2] <- "nobs"
}


