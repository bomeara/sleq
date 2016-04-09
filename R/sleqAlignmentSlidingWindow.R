CountBasesAtSite <- function(x) {
  length(unique(x))
}

#Over each column, counts the unique number of bases/DNA symbols per column
#?how do we want to deal with gaps/n bases?
CountBasesAlongAlignment <- function(sequences) {
  return(apply(sequences$sequences, 2, FUN=CountBasesAtSite))
}

#CountBasesAlongAlignment(mice.seqalignment$sequences) #check to see it works. even with dims stille needs
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


IdentifyBadSites(mice.seqalignment, width=6, threshold=2)# does ID bad sites work


#' Removes poorly aligned areas of an alignment using a sliding window
#' @param sequences matrix of sequences from an object of class seqalignment
#' @param width integer for the width of the sliding window
#' @param threshold value for the cutoof of average number of bases needed for removal from alignment
#' @param by.gene logical as to whether to use sliding window to eliminate bad parts of an alignment by gene or for the whole alignment
#PurgeBadSites <- function(sequences, width=6, threshold=4) {
#  bad.sites <- IdentifyBadSites(sequences, width, threshold)
#  final.matrix <- sequences
#  if(length(bad.sites)>0) {
#    final.matrix <- sequences[,-bad.sites]
#  }
#  return(final.matrix)
#}

#######do it for multiple genes#########
#if by gene, perform for every gene by gene, otherwise slide over whole alignment
#default is TRUE as if concat. alignment, then it is like one gene
#for every gene, go through sliding window and identify bad sites
#remove bad sites and store in appendable object the good sites

PurgeBadSites <- function(sequences, width=6, threshold=4, by.gene=TRUE) {
  if (by.gene==TRUE){
    vec<-vector()
    for(index in unique(mice.seqalignment$genes)){
      bad.sites <- IdentifyBadSites(sequences[,which(sequences$genes==index)], width, threshold)
      final.matrix <- sequences
      vec<-append(vec, bad.sites)
      if(length(bad.sites)>0) {
        final.matrix <- sequences[,-bad.sites]
      }
    }
    return(final.matrix)
  }
  else{
    bad.sites <- IdentifyBadSites(sequences, width, threshold)
    final.matrix <- sequences$sequences
    if(length(bad.sites)>0) {
      final.matrix <- sequences[,-bad.sites]
    }
  }
  return(final.matrix)
}

pp<-PurgeBadSites(mice.seqalignment, width=6, threshold=2, by.gene=TRUE)

mice.seqalignment
mice.seqalignment$genes[483:965] = "2"


PurgeBadSites <- function(sequences, width=6, threshold=3, by.gene=TRUE) {
  if (by.gene==TRUE){
    bad.sites.gene<-vector()
    final.matrix<-sequences
    gene.position <- 0
    for(index in unique(sequences$genes)){
      local.gene.seqalignment <- subset.seqalignment(sequences, keep.gene = index)
      bad.sites<-IdentifyBadSites(local.gene.seqalignment, width = width, threshold = threshold)
      bad.sites.gene<-append(bad.sites.gene, bad.sites + gene.position)
      gene.position <- dim(local.gene.seqalignment)[2]
    }
    print(bad.sites.gene)
    if(length(bad.sites)>0) {
      final.matrix <- subset.seqalignment(sequences, kill.site = bad.sites.gene)#sequences[,-bad.sites]
    }
    return(final.matrix)
  }
  else{
    bad.sites <- IdentifyBadSites(sequences, width, threshold)
    final.matrix <- sequences$sequences
    if(length(bad.sites)>0) {
      final.matrix <- sequences[,-bad.sites]
    }
  }
  return(final.matrix)
}

