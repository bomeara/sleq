#' Create seqalignment class object
#' @param x A character matrix
#' @return A seqalignment object
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
	} else if (all(states %in% c("a", "b", "c", "d", "e", "f", "g", "h", "i", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z", "-"))) {
		seq.type <- "aa"
	}
	return(seq.type)
}


#Trying to override [] to go directly into the sequences. not working yet
# [.seqalignment <- function(x, ...) {
	# return(x$sequences[...])
# }

