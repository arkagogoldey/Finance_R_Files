#Assignment 1 Part 1 Hisam Sabouni
pollutantmean<-function(directory,pollutant, id=1:332){
	##read in all the data as csv files
	files_full<-list.files(directory,full.names=TRUE)
	##create an empty data frame to save data in
	data<-data.frame()
	##loop through all csv files and load them into data frame through rows
	for(i in id){
		data<-rbind(data,read.csv(files_full[i]))
	}
	data_id <- data[data$ID %in% id,]
	mean<-mean(data_id[[pollutant]],na.rm=TRUE)
}
setwd("//Users//hisamsabouni2//Desktop//R//Lectures//Assignment1")
x<-pollutantmean("specdata","sulfate",1:10)
x
