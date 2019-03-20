library(d3heatmap)
library(ggplot2)
library(gplots)
library(officer)
library(scales)
library(xlsx)

options(stringsAsFactors=FALSE)

gene_distribution=function(file1,file2,my_pos,my_chr){

	print(paste0("file1","::",file1))
	print(paste0("file2","::",file2))
	print(paste0("my_pos","::",my_pos))
	print(paste0("my_chr","::",my_chr))
	
	if(exists("d1")==FALSE){
		d1<<-read.xlsx(file1,1)
	}
	if(exists("d2s1")==FALSE){
		d2s1<<-read.xlsx(file2,1)
		d2s2<<-read.xlsx(file2,2)
		d2s3<<-read.xlsx(file2,3)
	}
	
	if(my_pos==1){			d2=d2s1	}
	else if(my_pos==2){		d2=d2s2 }
	else{				d2=d2s3 }
	
	d2[,2]=paste0("chr",d2[,2])
	d1[,1]=paste0("i_",d1[,1])
	u_chr=my_chr
	D=subset(d1,d1[,3]==u_chr)
	E=subset(d2,d2[,2]==u_chr)
	D=subset(D,D[,1]%in%E[,1])
	xx=hist(D[,4],breaks=100,main=u_chr,col="red")
	return(xx)
}