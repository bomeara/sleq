
#' Combines seqalignment objects and drops duplicate taxa 
#' @param ... seqalignment objects
#' @return A seqalignment object
#' @export
cbind.seqalignment <- function(...) {
	raw.seq.objects <- list(...)
# put intro warning stuff here	
	if(length(raw.seq.objects)<2) {
		warning("Need multiple objects to concatenate")
	}
	
	for(i in sequence(length(raw.seq.objects))){
		if(all(raw.seq.objects[[i]]$type[1]!=raw.seq.objects[[i]]$type)){
			warning("Type of provided objects are different")
		}
		seq.matrix <- data.frame(taxa=names(raw.seq.objects[[1]]), sequences=raw.seq.objects[[1]]$sequences, stringsAsFactors=FALSE) 
	new.type.vector <- raw.seq.objects[[1]]$type
	new.genes.vector <- raw.seq.objects[[1]]$genes
	new.pos.vector <- raw.seq.objects[[1]]$pos
	for(i in 2:length(raw.seq.objects)){
		seq.matrix <- merge(seq.matrix, data.frame(taxa=names(raw.seq.objects[[i]]), sequences=raw.seq.objects[[i]]$sequences, stringsAsFactors=FALSE), by="taxa", all=TRUE)
		new.type.vector <- c(new.type.vector, raw.seq.objects[[i]]$type)
		new.genes.vector <- c(new.genes.vector, raw.seq.objects[[i]]$genes)
		new.pos.vector <- c(new.pos.vector, raw.seq.objects[[i]]$pos)
	}
}
	seq.char.matrix <- as.matrix(seq.matrix[,-1])
	seq.char.matrix <- unname(seq.char.matrix)
	rownames(seq.char.matrix) <- seq.matrix$taxa
	seq.char.matrix[is.na(seq.char.matrix)] <- "-"
	new.seq.class.obj <- structure(list(
		sequences=seq.char.matrix, 
		type=new.type.vector, 
		genes=new.genes.vector, 
		pos=new.pos.vector), 
		class="seqalignment")
	return(new.seq.class.obj)
}



































