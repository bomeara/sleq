#' Provides length a sequence for each taxon in an alignment that is not a gap.
#' @param seq A character matrix or seqalignment object.
#' @param seq.type A value that indicates the type of sequences.
#' @examples
#' \dontrun{
#' test.sequences <- ((file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "fasta"))
#' test.sequences.lengths <- SeqLengthNoGaps(test.sequences, seq.typ="dna")
#' }
SeqLengthNoGaps <- function(seq, seq.type="dna"){
    if(class(seq) == "alignment") {
        seq.mat <- as.matrix.alignment(seq)
        seq.names <- seq$nam
    }
    if(class(seq) == "matrix") {
        seq.mat <- seq
        seq.names <- rownames(seq.mat)
    }
    if(class(seq) == "seqalignment"){
        seq.mat <- seq
        seq.names <- names(seq.mat)
    }
	res <- matrix(,dim(seq.mat)[1], 2)
	for(i in 1:dim(seq.mat)[1]){
		nucleo.table <- table(seq.mat[i,])
		res[i,1] <- seq.names[i]
        if(seq.type == "dna"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "t", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n")])
        }
        if(seq.type == "rna"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "u", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n")])
        }
        if(seq.type == "aa"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "b", "c", "d", "e", "f", "g", "h", "i", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z", '*')])
        }
	}
    seq.df <- data.frame(taxon=res[,1], site.data=as.numeric(res[,2]))
    return(seq.df)
}
