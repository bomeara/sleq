#' Create seqalignment class object
#' @param x A character matrix
#' @return A seqalignment object
#' @details
#' This class is, internally, a list containing character matrix \code{$sequences},
#' \code{$type} (a guess at what kind of sequence it is: dna, rna, or aa), and 
#' \code{$genes} (a character vector with labels for the genes; initialized at "1" for all.
#' However, one cool thing about this class is that if you use \code{[]} to access elements,
#' this returns elements from within the \code{$sequences} object, not the class object itself.
#' @examples 
#' library(ape)
#' data(woodmouse)
#' mice.raw <- as.character(woodmouse)
#' print(mice.raw[1:3, 1:20])
#' mice.seqalignment <- seqalignment(mice.raw)
#' print(mice.seqalignment[1:3, 1:20])
#' print(mice.seqalignment$type)
seqalignment <- function(x) {
  return(structure(
    list(
      sequences=x, 
      type=rep(GetSequenceType(x), dim(x)[2]),
      genes=rep("1", dim(x)[2]),
      pos=rep(NA, dim(x)[2])
    )
    , class="seqalignment")
  )
}

#' Get the type of a alignment
#' @param x A character matrix or seqalignment object
#' @return A text string describing the type
GetSequenceType <- function(x) {
	if(class(x)=="seqalignment") {
		x <- x$sequences
	}
	seq.type <- "unknown"
	states <- sort(unique(tolower(c(x))))
	if (all(states %in% c("a", "c", "g", "t", "-", " ", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n"))) {
		seq.type <- "dna"
	} else if (all(states %in% c("a", "c", "g", "u", "-", " ", "r", "y", "s", "w", "k", "m", "b", "d", "h", "v", "n"))) {
		seq.type <- "rna"
	} else if (all(states %in% c("a", "b", "c", "d", "e", "f", "g", "h", "i", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z", "-", '*'))) {
		seq.type <- "aa"
	}
	return(seq.type)
}


#' Using the bracket operator to go into the sequences inside a seqalignment
#' @param x A seqalignment object
#' @param ... The argument you pass to brackets
 '[.seqalignment' <- function(x, ...) {
	 return(x$sequences[...])
 }
 
#' Get a subset of a seqalignment object, returning another seqalignment object
#' @param keep.site A vector of site positions to retain
#' @param kill.site A vector of site positions to delete
#' @param keep.taxon A vector of taxa (names or numbers) to retain
#' @param kill.taxon A vector of taxa (names or numbers) to delete
#' @param keep.gene A vector of genes to retain
#' @param kill.gene A vector of genes to delete
#' @param keep.pos A vector of codon positions to retain
#' @param kill.pos A vector of codon positions to delete
#' @param keep.type A vector of types to retain
#' @param kill.type A vector of types to delete
#' @return A seqalignment object
#' @details
#' This will only filter on those arguments passed in: if you give it a list of keep.sites, it will include
#' all taxa, for example. You can filter on more than one feature at a time. The keep.* or kill.* arguments
#' let you filter for objects to keep or remove.
 subset.seqalignment <- function(x, keep.site=NULL, kill.site=NULL, keep.taxon=NULL, kill.taxon=NULL, keep.gene=NULL, kill.gene=NULL, keep.pos=NULL, kill.pos=NULL, keep.type=NULL, kill.type=NULL) {
 	cols.to.kill <- c()
 	if (!is.null(kill.site)) {
 		cols.to.kill <- c(cols.to.kill, kill.site)	
 	}
  	if (!is.null(keep.site)) {
 		cols.to.kill <- c(cols.to.kill, which(!(sequence(dim(x$sequences)[2]) %in% keep.site)))	
 	}
 	if (!is.null(keep.gene)) {
 		cols.to.kill <- c(cols.to.kill, which(!(x$gene %in% keep.gene)))
 	}
 	if (!is.null(kill.gene)) {
 		cols.to.kill <- c(cols.to.kill, which((x$gene %in% kill.gene)))
 	}
 	if (!is.null(keep.pos)) {
 		cols.to.kill <- c(cols.to.kill, which(!(x$pos %in% keep.pos)))
 	}
 	if (!is.null(kill.pos)) {
 		cols.to.kill <- c(cols.to.kill, which((x$pos %in% kill.pos)))
 	}
 	if (!is.null(keep.type)) {
 		cols.to.kill <- c(cols.to.kill, which(!(x$type %in% keep.type)))
 	}
 	if (!is.null(kill.type)) {
 		cols.to.kill <- c(cols.to.kill, which(!(x$type %in% keep.type)))
 	}
	cols.to.kill <- unique(cols.to.kill)
	if(length(cols.to.kill) > 0) {
		x$sequences <- x$sequences[,-cols.to.kill]
		x$genes <- 	x$genes[-cols.to.kill]
		x$type <- 	x$type[-cols.to.kill]
		x$pos <- 	x$pos[-cols.to.kill]
	}
	rows.to.kill <- c()
	if(!is.null(kill.taxon)) {
		if(is.numeric(kill.taxon)) {
			rows.to.kill <- kill.taxon	
		} else {
			rows.to.kill <- which(rownames(x$sequences) %in% kill.taxon)	
		}
	}
	if(!is.null(keep.taxon)) {
		if(is.numeric(keep.taxon)) {
			rows.to.kill <- which(!(sequence(dim(x$sequences)[1]) %in% keep.taxon))
		} else {
			rows.to.kill <- which(!(rownames(x$sequences) %in% keep.taxon))
		}	
	}
	rows.to.kill <- unique(rows.to.kill)
	if(length(rows.to.kill)>0) {
		x$sequences <- x$sequences[-rows.to.kill,]	
	}
	return(x)
 }

#' Override dim() for a seqalignment object to get dim of the sequences
#' @param x A seqalignment object
#' @return The output of dim on \code{$sequences}
dim.seqalignment <- function(x) {
	return(dim(x$sequences))	
}

#' Override names() for seqaligment object
#' @param x A seqalignment object
#' @return The output of rownames on \code{$sequences}
'names.seqalignment' <- function(x) {
	return(rownames(x$sequences))
}