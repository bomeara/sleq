#Testing SingleTaxonCompare function

#Example with woodmouse data


library(ape) 
data(woodmouse)
mice.raw<-as.character(woodmouse)
#source("seqalignment.R")
mice.seqalignment<-seqalignment(mice.raw)

sequence.example1<- mice.seqalignment$sequences[,c(12,14,30,960,963)]
#12,14, 30 960, 963

summary.sites<-SingleTaxonCompare(sequences=sequence.example1)
print(summary.sites)

# Other example Latrodectus hesperus 
#accession.number<-c("KP656931","DQ127326")
#library(ape)
#seqs.onespecies<- read.GenBank(access.nb=accession.number, species.names=TRUE,gene.names=TRUE, as.character=TRUE)
#sequences.example2<-rbind(seqs.onespecies[[1]][1:500], seqs.onespecies[[2]][1:500])
