#' @title Determine the best codon phase and return amino acid sequence
#' 
#' @name GetCodonPhase
#' 
#' @description \code{GetCodonPhase} determines the best translated amino acid sequence from a consensus DNA alignment by identifying the codon phase that produces the fewest number of stop codons. \code{translate} from the \link[seqinr] package is used to determine the positions of the stop codons in the consensus alignment in all three forward and three reverse codon phases, for a total of six codon phases. 
#' 
#' @usage GetCodonPhase(DNA.seq, numcode = 1, NAstring = "X", ambiguous = FALSE)
#' 
#' @param \code{DNA.seq} Vector of the consensus alignment of a DNA sequence. 
#' @param \code{numcode} The NCBI genetic code number used for translation. By default, the standard genetic code (1) is used. 
#' @param \code{NAstring} How to treat the translation of amino acids when there are ambiguous bases in codons. 
#' @param \code{ambiguous} Indicate whether amibiguous bases are considered. Set as FALSE as a default.  
#' 
#' @details 
#' \code{numStopCodons} integer list of the total number of stop codons in each of the six codon phases. 
#' \code{stopCodonPositions} character list of the beginning position for every stop codon found within each of the six codon phases. 
#' \code{translation} vector of the amino acid sequence from the codon phase with the fewest number of stop codons. 
#' 
#' @seealso \code{\link[seqinr]{translate}}
#' 
#' @examples 
#' \dontrun{
#' library(sleq)
#' 
#' malMfile <- system.file("sequences/malM.fasta", package = "seqinr")
#' malMseq <- read.fasta(malMfile)
#' 
#' # DNA sequence is stored under the 
#' malMtest <- GetCodonPhase(as.character(malMseq[[1]]))
#' }

GetCodonPhase <- function(DNA.seq, numcode = 1, NAstring = "X", ambiguous = FALSE){
stopPosList<-list()
translationList<-list()
frame<-c(0:2)
sens<-c("F","R")
for(frame.num in 0:2){
  for(sens.num in sequence(2)){
    result<-translate(seq=DNA.seq,frame=frame.num, sens = sens[sens.num], 
      numcode=numcode, NAstring=NAstring, ambiguous = ambiguous)
    stopPosList[[length(stopPosList) + 1]]<-grep("\\*",result)
    names(stopPosList)[[length(stopPosList)]]<-paste((frame.num + 1),"-",sens[sens.num],sep="")
    translationList[[length(translationList) + 1]]<-result
  }
}

numCodons<-lapply(stopPosList,length)
numCodons<-unlist(numCodons)

minVal<-which.min(numCodons)
numMin<-which(numCodons == minVal)
if(length(numMin) > 1){
  translation<-list()
  for(phase in numMin){
    translation[[length(translation) + 1]] <- translationList[[phase]]
    names(translation)[[length(translation)]]<-names(numCodons[phase])
  }
}else{
  translation<-translationList[[which.min(numCodons)]]
}

return(list("NumStopCodons"=numCodons,"stopCodonPostions"=stopPosList,
  "translation"=translation))
}
