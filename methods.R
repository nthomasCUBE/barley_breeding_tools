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
		D1=D
		E_M=E[,3]
		names(E_M)=E[,1]
		D=cbind(D,E_M[D[,1]])
		D[,4]=as.double(D[,4])
		D[,5]=as.double(D[,5])

		N=50
		c_bin=max(D[,4])/N
		X1=c(); X2=c()
		for(x in 1:N){
			my_s=(x-1)*c_bin; my_e=(x)*c_bin
			X1=c(X1,(my_s+my_e)/2.0)
			D_s=subset(D,D[,4]>=my_s & D[,4]<=my_e)
			if(dim(D_s)[1]>0){
				c_dist=max(D_s[,5])-min(D_s[,5])
			}else{
				c_dist=0
			}
			X2=c(X2,c_dist)
		}
		
		plot(X1,X2,type="o",cex=0,col="red",xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		#axis(1, at=1:length(X1), labels=X1/1000000)
		legend("top",c("MxS","MxB"),col=c("red","green"),pch=20,cex=1.5)

		d2=v$d2s3
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)
		E_M=E[,3]
		names(E_M)=E[,1]
		D=subset(D,D[,1]%in%E[,1])
		D=cbind(D,E_M[D[,1]])
		D[,4]=as.double(D[,4])
		D[,5]=as.double(D[,5])

		N=20
		c_bin=max(D[,4])/N
		X1=c(); X2=c()
		for(x in 1:N){
			my_s=(x-1)*c_bin; my_e=(x)*c_bin
			X1=c(X1,(my_s+my_e)/2)
			D_s=subset(D,D[,4]>=my_s & D[,4]<=my_e)
			if(dim(D_s)[1]>0){
				c_dist=max(D_s[,5])-min(D_s[,5])
			}else{
				c_dist=0
			}
			X2=c(X2,c_dist)
		}

		D2=D
		#xx=hist(D[,4],breaks=100,xlab="bp position chromosome",main=paste("MxB",u_chr),col="red",xlim=c(0,700000000),plot=FALSE)
		points(X1,X2,type="o",cex=0,col="green",xaxt='n',xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		#axis(1, at=1:length(X1), labels=X1/1000000)

		#pdf(paste0(u_chr,"_plot_",".pdf"),width=6,height=4)
		#par(mfrow=c(1,1))
		#xx1=hist(D1[,4],breaks=100,main=paste("MxS",u_chr),plot=FALSE)
		#xx2=hist(D2[,4],breaks=100,main=paste("MxB",u_chr),plot=FALSE)
		#plot(xx1$density,type="o",cex=0,col="red",xaxt='n',plot=FALSE,xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		#points(xx2$density,type="o",cex=0,col="green")
		#legend("top",c("MxS","MxB"),col=c("red","green"),pch=20,cex=1.5)
		#axis(1, at=1:length(xx$mids), labels=xx$mids/1000000)
		#dev.off()

	}else{
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)

		print(paste("9K physical map",dim(D)))
		print(paste("3 genetic map",dim(E)))
		D_not=subset(D,!(D[,1]%in%E[,1]))
		D=subset(D,D[,1]%in%E[,1])
		print(D_not)
		print(paste("remaining marker",dim(D)))
		
		xx=hist(D[,4],breaks=100,main=u_chr,col="red")
	}
	print("JA-5")

	shinyalert("gene distribution - end","gene distribution has ended",type="info")
	return(c())
}