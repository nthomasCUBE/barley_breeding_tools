library(d3heatmap)
library(ggplot2)
library(gplots)
library(officer)
library(scales)
library(xlsx)

options(stringsAsFactors=FALSE)

gene_distribution=function(file1,file2,my_pos,my_chr,v){

	shinyalert("gene distribution - start","gene distribution has started",type="info")
	
	print(paste0("file1","::",file1))
	print(paste0("file2","::",file2))
	print(paste0("my_pos","::",my_pos))
	print(paste0("my_chr","::",my_chr))
	
	print("JA-1")

	if(is.null(v[["d1"]])){
		v$d1=read.xlsx(file1,1)
	}

	print("JA-2")

	if(is.null(v[["d2s1"]])){
		v$d2s1=read.xlsx(file2,1)
		v$d2s2=read.xlsx(file2,2)
		v$d2s3=read.xlsx(file2,3)
	}

	print("JA-3")

	if(my_pos=="all"){		d2=v$d2s1 }
	else if(my_pos=="MxS"){		d2=v$d2s2 }
	else if(my_pos=="MxB"){		d2=v$d2s3 }
	
	print("JA-4")

	if(my_pos=="combined"){
	
		par(mfrow=c(1,2))

		d2=v$d2s2
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)
		D=subset(D,D[,1]%in%E[,1])
		xx=hist(D[,4],breaks=100,xlab="bp posotion chromosome",main=paste("MxS",u_chr),col="red")

		d2=v$d2s3
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)
		D=subset(D,D[,1]%in%E[,1])
		xx=hist(D[,4],breaks=100,xlab="bp posotion chromosome",main=paste("MxB",u_chr),col="red")
	}else{
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)
		D=subset(D,D[,1]%in%E[,1])
		xx=hist(D[,4],breaks=100,main=u_chr,col="red")
	}

	print("JA-5")

	shinyalert("gene distribution - end","gene distribution has ended",type="info")
	return(xx)
}