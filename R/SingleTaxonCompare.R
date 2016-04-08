#' Function that will summarize variability by site comparing multiple sequences for a single taxon
#' @param sequences Matrix of character sequences output of seqalignment ( seqalignmentoutput$sequences)
#' @return a list of matrices, one per site, summarizing the number and percentage of nucleotides
#' @examples
#' \dontrun{
#' SingleTaxonCompare<-function(sequences)
#' }

SingleTaxonCompare<-function(sequences){
no.seqs<-dim(sequences)[1]
no.sites<-dim(sequences)[2]
#summary.seqmatrix is a list that contains a matrix per entry, in that
#the basic stats of the site per nucleotidy are summarized
summary.seqmatrix<-list()

aux<-matrix(rep(0,12),ncol=2)
	row.names(aux)<-c("a","c","g","t","n","Total")
	colnames(aux)<-c("Number","%")

for(i in 1:no.sites){
	how.manya<-length(which(sequences[,i]=="a"))
	how.manyc<-length(which(sequences[,i]=="c"))
	how.manyg<-length(which(sequences[,i]=="g"))
	how.manyt<-length(which(sequences[,i]=="t"))
	how.manynucleotide<-c(how.manya, how.manyc, how.manyg, how.manyt)
	how.manyelse<- no.seqs-sum(how.manynucleotide)
	how.manynucleotide<-c(how.manynucleotide, how.manyelse, no.seqs)
	nucleotide.percentages<-how.manynucleotide/no.seqs
	aux[,1]<-how.manynucleotide
	aux[,2]<-nucleotide.percentages
	summary.seqmatrix[[i]]<-aux
}
weird.chars <- setdiff(sequences, c("a","c","g","t","n"))
if (length(weird.chars)!=0) {
	warning(paste("The sequences contain characters different from 'a', 'c', 'g', 't' or 'n'. They will be counted under 'n'. The characters in question are:", unique(weird.chars), sep=" "))
}
return(summary.seqmatrix)
}
