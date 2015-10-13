GetCodonPhase <- function(DNA.seq, numcode = 1, NAstring = "X", ambiguous = FALSE){
	stopPosList<-list()
	translationList<-list()
	frame<-c(0:2)
	sens<-c("F","R")
	for(frame.num in 0:2){
		for(sens.num in sequence(2)){
			result<-translate(seq=DNA.seq,frame=frame.num, sens = sens[sens.num], 
				numcode=numcode, NAstring=NAstring, ambiguous = ambiguous)
			stopPos<-grep("\\*",result)
			if(length(stopPos) == 0){
				stopPosList[[length(stopPosList) + 1]] <- 0
			}else{
				stopPosList[[length(stopPosList) + 1]]<-stopPos
			}
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