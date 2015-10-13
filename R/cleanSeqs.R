#' Removes sites that contain a certain percentage of missing data across taxa
#' @param seq A character matrix or seqalignment object
#' @param seq.type The type of sequences contained in the alignment.
#' @param cutoff The proportion of non-ambiguous data present to trim from the alignment.
#' @examples
#' \dontrun{
#' test.sequences <- read.alignment(file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "phylip")
#' test.sequences.clean <- CleanSeqs(test.sequences, seq.type="dna", cutoff=0.5)
#' test.sequences <- as.matrix(read.alignment(file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "phylip"))
#' test.sequences.clean <- CleanSeqs(test.sequences, seq.type="dna", cutoff=0.5)
#' }
CleanSeqs <- function(seq, seq.type="dna", cutoff=0.5){
    original <- SiteMissing(seq, seq.type=seq.type)
    sites.to.keep <- original[original[,2]>=cutoff,1]
    if(class(seq) == "alignment") {
        seq.mat <- as.matrix.alignment(seq)
        seq.mat.new <- seq.mat[,sites.to.keep]
    }
    if(class(seq) == "matrix") {
        seq.mat <- seq
        seq.mat.new <- seq.mat[,sites.to.keep]
    }
    if(class(seq) == "seqalignment"){
        seq.mat <- seq
        seq.mat.new <- subset.seqalignment(seq.mat, keep.site[,sites.to.keep])
    }
    return(seq.mat.new)
}


#' Get the proportion of non-ambiguous data across each site
#' @param seq A character matrix or seqalignment object.
#' @param seq.type The type of sequences contained in the alignment.
#' @return A matrix giving the site number and the percent data present at that site.
SiteMissing <- function(seq, seq.type="dna"){
    if(class(seq) == "alignment"){
        seq.mat <- as.matrix.alignment(seq)
    }else{
        seq.mat <- seq
    }
    res <- matrix(,dim(seq.mat)[2], 2)
    for(i in 1:dim(seq.mat)[2]){
        nucleo.table <- table(seq.mat[,i])
        res[i,1] <- i
        if(seq.type == "dna"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "t", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n")])/dim(seq.mat)[1]
        }
        if(seq.type == "rna"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "u", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n")])/dim(seq.mat)[1]
        }
        if(seq.type == "aa"){
            res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "b", "c", "d", "e", "f", "g", "h", "i", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z", '*')])/dim(seq.mat)[1]
        }
	}
	colnames(res) <- c("site", "percent.data")
    return(res)
}
