library(d3heatmap)
library(ggplot2)
library(gplots)
library(officer)
library(scales)
library(xlsx)

options(stringsAsFactors=FALSE)

gene_distribution=function(file1,file2){
	if(exists("d1")==FALSE){
		d1<<-read.xlsx(file1,1)
	}
	if(exists("d2")==FALSE){
		d2<<-read.xlsx(file2,1)
	}
	
	d2[,2]=paste0("chr",d2[,2])
	d1[,1]=paste0("i_",d1[,1])
	
	print(head(d1))
	print(head(d2))
	
	xx=hist(c(1,2,3,4,4))
	u_chr=unique(d1[,3])
	for(x in 1:length(u_chr)){
		print(u_chr[x])
		D=subset(d1,d1[,3]==u_chr[x])

		E=subset(d2,d2[,2]==u_chr[x])
		D=subset(D,D[,1]%in%E[,1])
		
		hist(D[,4],breaks=100,main=u_chr[x],col="red")
	}
	return(xx)
}