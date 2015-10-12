#' Removes sites that contain a certain percentage of missing data across taxa.
#' @param seq A character matrix or seqalignment object
#' @param cutoff The proportion of non-ambiguous data present to trim from the alignment.
#' @examples
#' \dontrun{
#' test.sequences <- ((file = system.file("sequences/test.phylip",
#' package = "seqinr"), format = "fasta"))
#' test.sequences.clean <- CleanSeqs(test.sequences, cutoff=0.5)
#' }
CleanSeqs <- function(seq, cutoff=0.5){
    original <- SiteMissing(seq)
    sites.to.keep <- original[original[,2]>=cutoff,]
    if(class(seq) == "alignment"){
        seq.mat <- as.matrix.alignment(seq)
        seq.mat.new <- seq.mat[,sites.to.keep]
    }else{
        seq.mat <- as.matrix.alignment(seq)
        seq.mat.new <- seq.mat[,sites.to.keep]
    }
    return(seq.mat.new)
}


#' Get the proportion of non-ambiguous data across each site
#' @param seq A character matrix or seqalignment object
#' @return A matrix giving the site number and the percent data present at that site.
SiteMissing <- function(seq){
    if(class(seq) == "alignment"){
        seq.mat <- as.matrix.alignment(seq)
    }
    res <- matrix(,dim(seq.mat)[2], 2)
    for(i in 1:dim(seq.mat)[2]){
        nucleo.table <- table(seq.mat[,i])
        res[i,1] <- i
        res[i,2] <- sum(nucleo.table[names(nucleo.table) %in% c("a", "c", "g", "t")])/dim(seq.mat)[1]
	}
	colnames(res) <- c("site", "percent.data")
    return(res)
}
