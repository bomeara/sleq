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
#' print(mice.raw[1:3, 1:20]
#' mice.seqalignment <- seqalignment(mice)
#' print(mice.seqalignment[1:3, 1:20])
#' print(mice.seqalignment$type)
seqalignment <- function(x) {
  return(structure(
    list(
      sequences=x, 
      type=GetSequenceType(x),
      genes=rep("1", dim(x)[2])
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
 '[.seqalignment' <- function(x, ...) {
	 return(x$sequences[...])
 }

