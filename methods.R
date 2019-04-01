library(d3heatmap)
library(ggplot2)
library(gplots)
library(officer)
library(scales)
library(xlsx)

options(stringsAsFactors=FALSE)

calc_allele=function(c_f,v){
	print("calc_allele")
	if(is.null(v[["a1"]])){
		print("loading...")
		v$a1=read.csv(c_f,sep=";")
		v$a1=v$a1[-1,]
		print(head(v$a1))
		print("finished...")
	}
}


gene_distribution=function(file1,file2,my_pos,my_chr,v,only_common){

	shinyalert("gene distribution - start","gene distribution has started",type="info")
	
	print(paste0("gene_distribution","started"))
	
	if(is.null(v[["d1"]])){
		v$d1=read.xlsx(file1,1)
		v$d1[,1]=gsub("BOPA1_|BOPA2_","",v$d1[,1])
		v$d1[,1]=paste0("i_",v$d1[,1])
	}

	if(is.null(v[["d2s1"]])){
		v$d2s1=read.xlsx(file2,1)
		v$d2s2=read.xlsx(file2,2)
		v$d2s3=read.xlsx(file2,3)
	}

	if(my_pos=="all"){		d2=v$d2s1 }
	else if(my_pos=="MxS"){		d2=v$d2s2 }
	else if(my_pos=="MxB"){		d2=v$d2s3 }

	if(my_pos=="combined"){
		par(mfrow=c(1,2))
		if(only_common=="true"){
			print("only_shared")
			my_shared=v$d2s2[,1]
			my_shared=my_shared[my_shared%in%v$d2s3[,1]]
			my_shared=unique(my_shared)
			r1=v$d2s2
			r1=subset(r1,r1[,1]%in%my_shared)
			r2=v$d2s3
			r2=subset(r2,r2[,1]%in%my_shared)
			d2s2_cpy=r1
			d2s3_cpy=r2
		}else{
			d2s2_cpy=v$d2s2
			d2s3_cpy=v$d2s3
		}
		d2=d2s2_cpy
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
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
		X11=c(); X12=c()
		for(x in 1:N){
			my_s=(x-1)*c_bin; my_e=(x)*c_bin
			X11=c(X11,(my_s+my_e)/2.0/1000000)
			D_s=subset(D,D[,4]>=my_s & D[,4]<=my_e)
			if(dim(D_s)[1]>0){
				c_dist=max(D_s[,5])-min(D_s[,5])
			}else{
				c_dist=0
			}
			X12=c(X12,c_dist)
		}
		
		plot(X11,X12,type="o",cex=0,col="red",xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		legend("top",c("MxS","MxB"),col=c("red","green"),pch=20,cex=1.5)

		d2=d2s3_cpy
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
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
		X21=c(); X22=c()
		for(x in 1:N){
			my_s=(x-1)*c_bin; my_e=(x)*c_bin
			X21=c(X21,(my_s+my_e)/2.0/1000000)
			D_s=subset(D,D[,4]>=my_s & D[,4]<=my_e)
			if(dim(D_s)[1]>0){
				c_dist=max(D_s[,5])-min(D_s[,5])
			}else{
				c_dist=0
			}
			X22=c(X22,c_dist)
		}
		D2=D
		points(X21,X22,type="o",cex=0,col="green",xaxt='n',xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)

		pdf(paste0(u_chr,"_",only_common,"_plot_",".pdf"),width=7,height=5)
		par(mfrow=c(1,1))
		plot(X11,X12,type="o",cex=0,col="red",xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		points(X21,X22,type="o",cex=0,col="green",xaxt='n',xlab="chromosome [Mbp]",ylab="recombination [cM]",main=u_chr)
		dev.off()
	}else{
		d1=v$d1
		d2[,2]=paste0("chr",d2[,2])
		d1[,1]=paste0("i_",d1[,1])
		u_chr=my_chr
		D=subset(d1,d1[,3]==u_chr)
		E=subset(d2,d2[,2]==u_chr)
		D_not=subset(D,!(D[,1]%in%E[,1]))
		D=subset(D,D[,1]%in%E[,1])
		xx=hist(D[,4],breaks=100,main=u_chr,col="red")
	}
	shinyalert("gene distribution - end","gene distribution has ended",type="info")
	return(c())
}