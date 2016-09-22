complete<-function(directory,id=1:332){
	##read in all the data as csv files
	files_full<-list.files(directory,full.names=TRUE)
##create an empty data frame to save data in
 	data<-data.frame()
 	##loop through all csv files and load them into data frame through rows
 	for(i in id){
 		data<-rbind(data,read.csv(files_full[i]))
 	}
 	##Return a data frame with ids
 	data_id <- data[data$ID %in% id,]
 	##break apart the ids 
 	nobs<- data.frame()
 	##Calculate the number of complete cases for each id
 	for (z in id){
 	 	nobs<- rbind(nobs,sum(complete.cases(subset(data_id, ID==z))))
 	}
 	#Change column name of nobs
 	colnames(nobs)<-"nobs"
 	#bind id and nobs together by column
 	cbind(id,nobs)
}
##Example
x<-complete("specdata",3)
x
#It works!