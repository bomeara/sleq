#' Provides sequence length for each taxon in an alignment.
#' @param seq A character matrix or seqalignment object
#' @examples
#' \dontrun{
#' test.sequences <- ((file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "fasta"))
#' test.sequences.lengths <- SeqLength(test.sequences)
#' }
SeqLength <- function(seq){
    if(class(seq) == "alignment"){
        seq.mat <- as.matrix.alignment(seq)
    }
	res <- matrix(,dim(seq.mat)[1], 2)
	for(i in 1:dim(seq.mat)[1]){
		nucleo.table <- table(seq.mat[i,])
		res[i,1] <- seq$nam[i]
		res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "t")])
	}
	colnames(res) <- c("taxon", "site.data")
    return(res)
}
