#' @title Determine the best codon phase and return amino acid sequence
#' 
#' @name GetCodonPhase
#' 
#' @description \code{GetCodonPhase} determines the best translated amino acid sequence from a consensus DNA alignment by identifying the codon phase that produces the fewest number of stop codons. \code{translate} from the seqinr package is used to determine the positions of the stop codons in the consensus alignment in all three forward and three reverse codon phases, for a total of six codon phases. 
#' 
#' @usage GetCodonPhase(DNA.seq, numcode = 1, NAstring = "X", ambiguous = FALSE)
#' 
#' @param \code{DNA.seq} Vector of the consensus alignment of a DNA sequence. 
#' @param \code{numcode} The NCBI genetic code number used for translation. By default, the standard genetic code (1) is used. 
#' See NCBI Genetic Code website for more details: \url{http://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=t}
#' @param \code{NAstring} How to treat the translation of amino acids when there are ambiguous bases in codons. 
#' @param \code{ambiguous} Indicate whether ambiguous bases are considered. Set as FALSE as a default.   
#' @param \code{return.all} In case of multiple equally good phases, return all (TRUE, the default) or just one at random.
#' @details 
#' \code{numStopCodons} integer list of the total number of stop codons in each of the six codon phases. 
#' \code{stopCodonPositions} character list of the beginning position for every stop codon found within each of the six codon phases. 
#' \code{translation} vector of the amino acid sequence from the codon phase with the fewest number of stop codons. 
#' 
#' @seealso \code{translate} in seqinr package
#' 
#' @examples 
#' \dontrun{
#' malMfile <- system.file("sequences/malM.fasta", package = "seqinr")
#' malMseq <- read.fasta(malMfile)
#'
#' # DNA sequence is stored under the 
#' malMtest <- GetCodonPhase(as.character(malMseq[[1]]))
#' 
#' ### Example using multiple sequences, and then creating a consensus alignment
# Load in sample alignment
#' fastaf <- system.file("sequences/Anouk.fasta", package = "seqinr")
#' sample.fasta <- read.alignment(file = system.file("sequences/Anouk.fasta", package = "seqinr"), format = "fasta")
#'
#' # Get consensus
#' Anouk <- consensus(sample.fasta, method = "majority")
#' }
GetCodonPhase <- function(DNA.seq, numcode = 1, NAstring = "X", ambiguous = FALSE, return.all=TRUE){
	stopPosList<-list()
	stopPosCounts <- rep(NA, 6)
	translationList<-list()
	frame<-c(0:2)
	sens<-c("F","R")
	for(frame.num in 0:2){
		for(sens.num in sequence(2)){
			result<-translate(seq=DNA.seq,frame=frame.num, sens = sens[sens.num], 
				numcode=numcode, NAstring=NAstring, ambiguous = ambiguous)
			stopPos<-grep("\\*",result)
			if(length(stopPos) == 0){
				stopPosList[[length(stopPosList) + 1]] <- "" #doing zero here counts it as a stop codon below
			}else{
				stopPosList[[length(stopPosList) + 1]]<-stopPos
			}
			stopPosCounts[length(stopPosList)] <- length(stopPos)
			names(stopPosList)[[length(stopPosList)]]<-paste((frame.num + 1),"-",sens[sens.num],sep="")
			translationList[[length(translationList) + 1]]<-result
		}
	}	

	indexMin <- which(stopPosCounts==min(stopPosCounts))
	if(!return.all) {
		indexMin <- sample(indexMin, 1)	
	}
	if(length(indexMin) > 1){
		translation<-list()
		for(phase.index in sequence(length(indexMin))){
			phase <- indexMin[phase.index]
			translation[[length(translation) + 1]] <- translationList[[phase]]
			names(translation)[[length(translation)]]<-names(stopPosList[phase])
		}
	}else{
		translation<-translationList[[indexMin]]
	}
	
	return(list("NumStopCodons"= stopPosCounts,"stopCodonPostions"=stopPosList,
		"translation"=translation))
}
