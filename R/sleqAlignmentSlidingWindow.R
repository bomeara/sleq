

#requires#
#library(zoo)

mice.seqalignment

CountBasesAtSite <- function(x) {
  length(unique(x))
}

#Over each column, counts the unique number of bases/DNA symbols per column
#?how do we want to deal with gaps/n bases?
CountBasesAlongAlignment <- function(sequences) {
  return(apply(sequences, 2, FUN=CountBasesAtSite))
}

CountBasesAlongAlignment(mice.seqalignment$sequences) #check to see it works. even with dims stille needs
#the operator for sequences


#width is the length of the viewing window across the sequences, i.e.width=6 would slide pos. 1:6, 2:7 to length(x) of alignement
CountRollingAverageNumberBases <- function(sequences, width=6) {
  return(rollapply(CountBasesAlongAlignment(sequences), width=width, FUN=mean, align="left"))
}

#CountRollingAverageNumberBases(mice.seqalignment$sequences, width=6)#test average per window

#threshold is how many bases at a site to consider it bad
IdentifyBadSites <- function(sequences, width=6, threshold=3) {
  scores <- CountRollingAverageNumberBases(sequences, width=width)
  scores <- c(scores, rep(scores[length(scores)], width-1))
  return(which(scores >= threshold))
}


IdentifyBadSites(mice.seqalignment$sequences, width=6, threshold=2)# does ID bad sites work


#' Removes poorly aligned sequences using a sliding window
#' @param sequences matrix of sequences from an object of class seqalignment
#' @param width integer for the width of the sliding window
#' @param threshold value for the cutoof of average number of bases needed for removal from alignment
PurgeBadSites <- function(sequences, width=6, threshold=4) {
  bad.sites <- IdentifyBadSites(sequences, width, threshold)
  final.matrix <- sequences
  if(length(bad.sites)>0) {
    final.matrix <- sequences[,-bad.sites]
  }
  return(final.matrix)
}




