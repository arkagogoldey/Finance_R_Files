corr<-function(directory, threshold=0){
	#gather all data in the directory
	files_list<-list.files(directory,full.names=TRUE)
	data<-lapply(files_list,read.csv)
	#threshold values..
	y<-as.data.frame(files_list)
	z<-data.frame()
	for (i in 1:nrow(y)){
		z<-rbind(z,sum(complete.cases(data[[i]])))
	}
	colnames(z)<-"obs"
	id<-c(1:nrow(y))
	h<-cbind(id,z)
	x<-c(h$obs>threshold)
	x<-cbind(id,x)
	colnames(x)<-c("id","complete")
	x<-as.data.frame(x)
	x<-subset(x,complete>0)
	final<-data.frame()
	final<-lapply(files_list[x$id],read.csv)
	correlations<-numeric()
	if(length(final)>0){
	for(i in 1:length(final)){
		correlations<-cbind(correlations,cor(final[[i]]$sulfate,final[[i]]$nitrate, use="pairwise.complete.obs"))
	}
	}else{
		print(0)
		}
	correlations<-as.vector(correlations)
}

y<-corr("specdata",5000)
head(y)
length(y)