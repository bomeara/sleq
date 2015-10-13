# Other example Latrodectus hesperus 
accession.number<-c("KP656931","DQ127326","KP656707","KC414081","KC414080")
library(ape)
#source("~/Dropbox/sleq/R/SingleTaxonCompare.R")
seqs.onespecies<- read.GenBank(access.nb=accession.number, species.names=TRUE,gene.names=TRUE, as.character=TRUE)

seqs.onespeciesv2<- read.GenBank(access.nb=accession.number, species.names=TRUE,gene.names=TRUE, as.character=FALSE)

no.sites<-500
no.seqs<-5

sequences.example2<-rbind(seqs.onespecies[[1]][1:500], seqs.onespecies[[2]][1:500], seqs.onespecies[[3]][1:500], seqs.onespecies[[4]][1:500],seqs.onespecies[[5]][1:500] )

summary.example2<-SingleTaxonCompare(sequences.example2)

print(summary.example2[1:10])
#3 474
#
percentage.rankmatrix<-matrix(rep(0,no.sites*4), nrow=4)
nucleotide.rankmatrix<-matrix(rep("l",no.sites*4),nrow=4)
rank.qualityseq<-rep(0,no.seqs)

for(i in 1:10){
aux<-sort(summary.example2[[i]][1:4,2],decreasing=TRUE)
percentage.rankmatrix[,i]<-aux
nucleotide.rankmatrix[,i]<-names(aux)

aux1<-match(percentage.rankmatrix[,i],percentage.rankmatrix[,i])
aux2<-which(aux1==1)
target.nucleotide<-nucleotide.rankmatrix[aux2,i]
case.number<-length(target.nucleotide)
if(case.number==1){
		penalty.value<-1
}else{
	if(case.number==2){
		penalty.value<-0.5
	}else{
		if(case.number==3){
			penalty.value=0.3
		}else{ 
			penalty.value=0
		}
	}	
}

target.seqsite<-which(sequences.example2[,i]%in%target.nucleotide)
rank.qualityseq[target.seqsite]<-rank.qualityseq[target.seqsite]+penalty.value
}



a=percentage.rankmatrix[,3]
match(a,a)
summary.example2[1:5]

